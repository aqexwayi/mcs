(ns mcs.blockclass
  (:require [mcs.util :as util])
  (:require [mcs.rk4 :as rk4])
  (:require [mcs.tf :as tf]))

(declare block-classes)
(defn get-block-parameter [block name]
  (first (filter #(= (:name %) name) (:inputs block))))

(defn get-block-state [ctx block-id]
  (let [v (get (:blocks-state ctx) block-id)]
    v))

(defn set-block-state [ctx block-id m]
  (update-in ctx [:blocks-state] assoc block-id m))

(defn merge-block-state [ctx block-id m]
  (let [m0 (get-block-state ctx block-id)]
    (update-in ctx [:blocks-state] assoc block-id (merge m0 m))))

(defn get-blocks-before [ctx offset]
  (let [bs (:blocks-value ctx)]
    (if (> (count bs) offset)
      (nth bs offset)
      (last bs))))

(defn block-class-from-type-name [block-type]
  (first (filter #(= block-type (:type-name %)) block-classes)))

;; return block or nil
(defn find-block-by-id [id-str bs]
  (let [id (Integer/parseInt id-str)
        f (fn [b]
            (let [bc (block-class-from-type-name (:block-type b))
                  bid (Integer/parseInt (:block-id b))
                  next-bid (+ bid (count (:outputs bc)))]
              (and (<= bid id) (< id next-bid))))]
    (first (filter f bs))))

(defn find-parameter-by-name [ps name]
  (first (filter #(= name (:name %)) ps)))

(defn get-block-default-value [ctx block-id]
  (if-let [b (find-block-by-id block-id (:blocks ctx))]
    (if-let [para (find-parameter-by-name (:inputs b) "DEFAULT")]
      (get para :value)
      (let [x1 (Integer/parseInt block-id)
            x0 (Integer/parseInt (:block-id b))
            bc (block-class-from-type-name (:block-type b))
            ot (nth (:outputs bc) (- x1 x0))]
        (case ot
          :real 0.0
          :bool false
          nil)))))

(defn block-value-computed? [ctx block-id]
  (let [bs (get-blocks-before ctx 0)]
    (not (nil? (get bs block-id)))))

;; if this block is not in current blocks-value table
;; we must use the value one clock earlier.
;; this happen when
;; (1) current simulation block.
;; (2) input block with larger block id.
(defn get-block-value- [ctx block-id offset]
  (if (block-value-computed? ctx block-id) 
    (get (get-blocks-before ctx offset) block-id)
    (get (get-blocks-before ctx (inc offset)) block-id)))

(defn get-block-value [ctx block-id offset]
  (let [v (get-block-value- ctx block-id offset)]
    (if (nil? v)
      (let [v (get-block-default-value ctx block-id)]
        (if (nil? v)
          (if (some #{block-id} (:di ctx))
            false
            (if (some #{block-id} (:ai ctx))
              0.0
              nil))
          v))
      v)))

;; if you want to use 1 to index the last value of the block , 
;; you can't pass 1 to get-block-value directly. because 
;; current simulating block is not in current blocks-value table 
;; you must call (get-block-value ctx bid 0)
(defn get-current-block-value [ctx block-id offset]
  (get-block-value ctx block-id (dec offset)))

(defn get-block-input-value [ctx block name]
  (if-let [para (get-block-parameter block name)]
    (if (get para :link)
      (get-block-value ctx (:link-block-id para) 0)
      (get para :value))))

(defn get-block-input-link [ctx block name]
  (if-let [para (get-block-parameter block name)]
    (:link-block-id para)))

(defn get-parameter-array-value [ctx block name len]
  (let [rng (range 1 (inc len))]
    (map #(get-block-input-value ctx block (str name %))
         rng)))

(defn update-context [ctx m]
  (let [ms (:blocks-value ctx)
        m0 (first ms)
        ms2 (cons (merge m0 m) (rest ms))]
    (assoc ctx :blocks-value ms2)))

(defn keep-block-value [ctx bid]
  (let [last-v (get-current-block-value ctx bid 1)]
    (update-context ctx {bid last-v})))

(defn schedule-timer [ctx bid offset action]
  (update-in ctx [:timers] assoc bid [offset action]))

(defn cancel-timer [ctx bid]
  (update-in ctx [:timers] dissoc bid))

(defn run-timer [ctx]
  (let [ts (into () (:timers ctx))
        ff1 (fn [[id [off act]]] [id [(dec off) act]])
        ts2 (map ff1 ts)
        [ts3 ts4] (split-with #(> 0 (first (second %))) ts2)
        ff2 (fn [[id [off act]]]
              [id (case act 
                    :set true 
                    :reset false
                    nil)])
        ts5 (map ff2 ts3)
        ff3 (fn [[id value]]
              [(str (inc (Integer/parseInt id))) (not value)])
        ts6 (map ff3 ts5) 
        m (into {} (concat ts5 ts6))
        ctx2 (update-context ctx m)
        ]
    (assoc ctx2 :timers (into {} ts4))))

(defn get-clock-value-in-this-cycle [ctx block]
  (let [T (get-block-input-value ctx block "T")
        t (get ctx :clock 0.0)]
    (- t (* (int (/ t T)) T))))

(defn get-ai-with-change-limited [last-value target-value max-dv]
  (if (< last-value target-value)
    (let [v (+ last-value max-dv)]
      (util/limit-value v last-value target-value))
    (let [v (- last-value max-dv)]
      (util/limit-value v target-value last-value))))

(defn interpolate-arr [arr-x arr-y x]
  (let [pos (count (filter #(< % x) arr-x))]
    (cond
     (= pos 0) (first arr-y)
     (= pos (count arr-x)) (last arr-y)
     :else (let [x1 (nth arr-x (dec pos))
                 x2 (nth arr-x pos)
                 y1 (nth arr-y (dec pos))
                 y2 (nth arr-y pos)]
             (util/interpolate [x1 y1] [x2 y2] x)))))

(defn piecewise-function [ctx block]
  (let [ai (get-block-input-value ctx block "AI")
        n (get-block-input-value ctx block "N")
        arr-x (get-parameter-array-value ctx block "X" n)
        arr-y (get-parameter-array-value ctx block "Y" n)
        ao (interpolate-arr arr-x arr-y ai)
        ]
    [ao]))
    
(defn limit-change-rate [ctx block]
  (let [ai (get-block-input-value ctx block "AI")
        dv (get-block-input-value ctx block "DV")
        uv (get-block-input-value ctx block "UV")
        interval (:interval ctx)
        ]
    (if (get-block-input-value ctx block "TS")
      [ai]
      (let [bid (:block-id block)
            last-v (get-current-block-value ctx bid 1)
            l (- last-v (* dv interval))
            h (+ last-v (* uv interval))]
        [(util/limit-value ai l h)]))))

(defn lag [ctx block]
  (let [bid (:block-id block)
        rid (get-block-input-link ctx block "AI")
        K (get-block-input-value ctx block "K")
        T (util/not-zero (get-block-input-value ctx block "LAG"))
        dt (:interval ctx)
        c1 (get-current-block-value ctx bid 1) ;; c[-1]
        r0 (get-block-value ctx rid 0)
        r1 (get-block-value ctx rid 1)
        e (Math/pow Math/E (* -1.0 (/ dt T)))
        c0 (+ (* e c1) (* (- 1.0 e) K r0))]
    [c0]))

(defn lag2 [ctx block]
  (if (get-block-input-value ctx block "TS")
    [(get-block-input-value ctx block "AI")]
    (lag ctx block)))

(defn lead-lag [ctx block]
  (let [bid (:block-id block)]
    (if (get-block-input-value ctx block "TS")
      (let [v (get-block-input-value ctx block "AI")]
        (update-context ctx {bid v}))
      (let [dt (:interval ctx)
            T1 (util/not-zero (get-block-input-value ctx block "LEAD"))
            T2 (util/not-zero (get-block-input-value ctx block "LAG"))
            K (get-block-input-value ctx block "K")
            rid (get-block-input-link ctx block "AI")
            rate (get-block-input-value ctx block "RATE")
            r0 (get-block-value ctx rid 0)
            r1 (get-block-value ctx rid 1)
            r2 (get-block-value ctx rid 2)
            c1 (or (get-block-state ctx bid)
                   (get-block-default-value ctx bid))
            e (Math/pow Math/E (* -1.0 (/ dt T2)))
            c0 (+ (* e c1) (* K (+ (* (/ T1 T2) (- r0 r1)) (* (- 1.0 e) r1))))
            ctx2 (set-block-state ctx bid c0)
            v1 (get-current-block-value ctx bid 1)
            l (- v1 (* rate dt))
            h (+ v1 (* rate dt))
            v0 (util/limit-value c0 l h)
            ]
        (update-context ctx2 {bid v0})))))

(defn differential [ctx block]
  (let [rid (get-block-input-link ctx block "AI")
        bid (:block-id block)
        dt (:interval ctx)
        Td (util/not-zero (get-block-input-value ctx block "TD"))
        Kd (get-block-input-value ctx block "KD")
        r0 (get-block-value ctx rid 0)
        r1 (get-block-value ctx rid 1)
        r2 (get-block-value ctx rid 2)
        c1 (or (get-block-state ctx bid)
               (get-block-default-value ctx bid))
        e (Math/pow Math/E (* -1.0 (/ dt Td)))
        c0 (+ (* e c1) (* Kd (- r0 r1)))
        ctx2 (set-block-state ctx bid c0)
        h (get-block-input-value ctx block "H")
        l (get-block-input-value ctx block "L")
        v (util/limit-value c0 l h)
        ]
    (update-context ctx2 {bid v})))

(defn transfer-function [ctx block]
  (let [bid (:block-id block)
        num (get-block-input-value ctx block "NUM")
        den (get-block-input-value ctx block "DEN")
        n (dec (count den))
        num+ (reverse (take (inc n) (concat (reverse num) (repeat 0))))
        f #(/ % (double (first den)))
        b (map f num+) 
        a (map f den)  ;; a0 == 1.0
        M (or (get-block-state ctx bid)
              {:X nil :Y 0.0 :num b :den a})
        dt (:interval ctx)
        u (get-block-input-value ctx block "AI")
        M+ (tf/step M u dt)
        ctx2 (set-block-state ctx bid M+)
        ctx3 (update-context ctx2 {bid (:Y M+)})
        ]
    ctx3))

(defn latch-inc-dec [v last-v bi bd]
  (case [bi bd]
        [true true] last-v
        [true false] (min v last-v)
        [false true] (max v last-v)
        [false false] v
        :else (println "bi=" bi ",bd=" bd)
    ))

(defn epid [ctx block]
  (let [bid (:block-id block)
        bid1 (str (inc (Integer/parseInt bid)))
        bid2 (str (inc (Integer/parseInt bid1)))
        dt (:interval ctx)
        pv (get-block-input-value ctx block "PV")
        sp (get-block-input-value ctx block "SP")
        Tr (get-block-input-value ctx block "TR")
        Ts (get-block-input-value ctx block "TS")
        Ti (get-block-input-value ctx block "TI")
        Kp (get-block-input-value ctx block "KP")
        Td (get-block-input-value ctx block "TD")
        Kd (get-block-input-value ctx block "KD")
        Kf (get-block-input-value ctx block "KF")
        ff (get-block-input-value ctx block "FF")
        edb (get-block-input-value ctx block "EDB")
        dir (get-block-input-value ctx block "DIR")
        Ch (get-block-input-value ctx block "CH")
        Cl (get-block-input-value ctx block "CL")
        BI (get-block-input-value ctx block "BI")
        BD (get-block-input-value ctx block "BD")
        idev1 (get-block-input-value ctx block "IDEV1")
        idev2 (get-block-input-value ctx block "IDEV2")
        beta (get-block-input-value ctx block "BETA")
        ]
    (let [err (if dir (- pv sp) (- sp pv))]
     (if Ts
       (let [tr (util/limit-value Tr Cl Ch)
             _ (println "tr=" tr ",ff=" ff)
             ctx2 (set-block-state ctx bid {"ERR" err
                                            "I" (/ (- tr ff (* Kp err)) Kp)
                                            "D" 0.0
                                            "OUTPUT" tr})]
         (update-context ctx2 {bid Tr 
                               bid1 (>= Tr Ch)
                               bid2 (<= Tr Cl)}))
       (let [st (get-block-state ctx bid)
             last-err (get st "ERR" 0.0)
             last-I (get st "I" 0.0)
             last-D (get st "D" 0.0)
             last-output (get st "OUTPUT" 0.0)
             last-bi (or (get-current-block-value ctx bid1 1) false)
             last-bd (or (get-current-block-value ctx bid2 1) false)
             bi (or BI last-bi)
             bd (or BD last-bd)
             prop err
             integ0 (if (util/almost-zero? Ti)
                      0.0
                      (* (/ dt Ti) err))
             integ1 (cond
                     (and bi (> err 0.0)) 0.0
                     (and bd (< err 0.0)) 0.0
                     (< (- idev1) err idev1) integ0
                     (< (- idev2) err idev2) (* beta integ0)
                     :else 0.0)
             integ (+ last-I integ1)
             diff (/ (+ (* Td last-D) (* Kd Td (- err last-err))) (+ dt Td))
             u0 (* Kp (+ prop integ diff))
             u1 (util/dead-zone u0 edb)
             u2 (+ u1 (* Kf ff)) 
             lb (if bd last-output Cl)
             ub (if bi last-output Ch)
             u3 (util/limit-value u2 lb ub)
             ctx2 (set-block-state ctx bid {"ERR" err
                                            "I" integ
                                            "D" diff
                                            "OUTPUT" u3})]
         (update-context ctx2 {bid u3
                               bid1 (or BI (>= u3 Ch)) 
                               bid2 (or BD (<= u3 Cl))}))))))

(defn output-value-parameter [ctx block]
  [(get-block-input-value ctx block "VALUE")])

(defn switching-ai [ctx block]
  (let [bid (:block-id block)
        last-v (get-current-block-value ctx bid 1)
        sw (get-block-input-value ctx block "SW")
        t (if sw
            (get-block-input-value ctx block "AI2")
            (get-block-input-value ctx block "AI1"))
        r (if sw 
            (get-block-input-value ctx block "R12")
            (get-block-input-value ctx block "R21"))
        max-dv (* r (:interval ctx))
        nv (get-ai-with-change-limited last-v t max-dv)
        ctx2 (set-block-state ctx bid {:steady? (util/almost-eq? nv t)
                                       :last-switch sw})
        ctx3 (update-context ctx2 {bid nv})
        ]
    ctx3))

(defn ai-switch [ctx block]
  (let [bid (:block-id block)
        st (get-block-state ctx bid) ;; steady state
        ss (or (nil? st) (:steady? st))
        sw0 (get-block-input-value ctx block "SW")
        sw1 (if (nil? st) sw0 (:last-switch st))
        ctx2 (set-block-state ctx bid {:steady? true
                                       :last-switch sw0})
        ]
    (if (and ss (= sw0 sw1))  ;; ss means steady state.
      (if sw0
        (update-context ctx2 {bid (get-block-input-value ctx block "AI2")})
        (update-context ctx2 {bid (get-block-input-value ctx block "AI1")}))
      (switching-ai ctx block))))

(defn hl-limit [ctx block]
  (let [l (get-block-input-value ctx block "L")
        h (get-block-input-value ctx block "H")
        ai (get-block-input-value ctx block "AI")
        v (util/limit-value ai l h)]
    [v]))

(defn hl-alarm [ctx block]
  (let [l (get-block-input-value ctx block "L")
        h (get-block-input-value ctx block "H")
        ai (get-block-input-value ctx block "AI")
        hw (> ai h)
        lw (< ai l)]
    [hw lw]))

(defn dev-alarm [ctx block]
  (let [l (get-block-input-value ctx block "L")
        h (get-block-input-value ctx block "H")
        ai1 (get-block-input-value ctx block "AI1")
        ai2 (get-block-input-value ctx block "AI2")
        dv (- ai1 ai2) ]
    [(not (< l dv h))]))

(defn wrap [f]
  (fn [ctx block]
    (let [bid (Integer/parseInt (:block-id block))
          res (f ctx block)
          ps (map-indexed (fn [idx v] 
                            [(str (+ idx bid)) v]) res)]
      (update-context ctx (into {} ps)))))

(def block-classes
  [{:type-name "函数发生器"
    :inputs [ {:name "AI" :desc "AI输入" :type :real :default 0.0 
               :link true :link-block-id "0"}
              {:name "N" :desc "函数分段数" :type :integer
               :min-value 1 :max-value 10 :default 2 :used-as-array-size true}
              {:name "X" :desc "X" :type :array :element-type :real :array-size "N"
               :default 1000000.0 }
              {:name "Y" :desc "Y" :type :array :element-type :real :array-size "N"
               :default 0.0 }
              ]
    :outputs [:real]
    :function (wrap piecewise-function)
    }
   {:type-name "模拟量设定"
    :inputs [{:name "VALUE" :desc "输出设定值" :type :real :default 0.0 }]
    :outputs [:real]
    :function (wrap output-value-parameter)
    }
   {:type-name "开关量设定"
    :inputs [{:name "VALUE" :desc "输出设定" :type :bool :default false}]
    :outputs [ :bool ]
    :function (wrap output-value-parameter)
    }
   {:type-name "超前滞后"
    :inputs [ {:name "LEAD" :desc "超前时间" :type :real :default 0.0 }
              {:name "LAG" :desc "滞后时间" :type :real :default 0.0 }
              {:name "K" :desc "增益" :type :real :default 1.0 }
              {:name "RATE" :desc "输出限速" :type :real :default 1.0
               :link false :link-block-id "0"}
              {:name "TS" :desc "跟踪标志" :type :bool :default false
               :link false :link-block-id "0"}
              {:name "AI" :desc "AI" :type :real :default 0.0
               :link true :link-block-id "0"}
              ]
    :outputs [ :real ]
    :function lead-lag
    } 
   {:type-name "一阶惯性"
    :inputs [ {:name "LAG" :desc "惯性时间" :type :real :default 10.0 }
              {:name "K" :desc "增益" :type :real :default 1.0 }
              {:name "AI" :desc "AI" :type :real :default 0.0
               :link true :link-block-id "0"}
              ]
    :outputs [ :real ]
    :function (wrap lag)
    } 
   {:type-name "一阶惯性跟踪"
    :inputs [ {:name "LAG" :desc "惯性时间" :type :real :default 10.0 }
              {:name "K" :desc "增益" :type :real :default 1.0 }
              {:name "TS" :desc "跟踪标志" :type :bool :default false
               :link true :link-block-id 0}
              {:name "AI" :desc "AI" :type :real :default 0.0
               :link true :link-block-id "0"}
              ]
    :outputs [ :real ]
    :function (wrap lag2)
    }
   {:type-name "微分"
    :inputs [ {:name "TD" :desc "微分时间" :type :real :default 10.0 }
              {:name "KD" :desc "微分增益" :type :real :default 8.0 }
              {:name "H" :desc "输出高限" :type :real :default 100.0 }
              {:name "L" :desc "输出低限" :type :real :default -100.0 }
              {:name "AI" :desc "AI" :type :real :default 0.0
               :link true :link-block-id "0"}
              ]
    :outputs [ :real ]
    :function differential
    }
   {:type-name "速率限制"
    :inputs [ {:name "UV" :desc "升速率" :type :real :default 100.0 }
              {:name "DV" :desc "降速率" :type :real :default 100.0 }
              {:name "TS" :desc "跟踪标志" :type :bool :default false }
              {:name "AI" :desc "AI" :type :real :default 0.0
               :link true :link-block-id "0"}
              ]
    :outputs [ :real ]
    :function (wrap limit-change-rate)
    }
   {:type-name "模拟量切换"
    :inputs [ {:name "R21" :desc "IN2 TO IN1速率" :type :real :default 100.0 }
              {:name "R12" :desc "IN1 TO IN2速率" :type :real :default 100.0 }
              {:name "SW" :desc "切换开关" :type :bool :default 0
               :link true :link-block-id 0}
              {:name "AI1" :desc "AI1" :type :real :default 0.0
               :link true :link-block-id "0"}
              {:name "AI2" :desc "AI2" :type :real :default 0.0
               :link true :link-block-id "0"}
              ]
    :outputs [ :real ]
    :function ai-switch
    }
   {:type-name "高低限幅"
    :inputs [ {:name "H" :desc "高限幅" :type :real :default 100.0 }
              {:name "L" :desc "低限幅" :type :real :default -100.0 }
              {:name "AI" :desc "AI" :type :real :default 0.0
               :link true :link-block-id "0"}
              ]
    :outputs [ :real ]
    :function (wrap hl-limit)
    }
   {:type-name "高低限幅报警"
    :inputs [ {:name "H" :desc "高限幅" :type :real :default 100.0 }
              {:name "L" :desc "低限幅" :type :real :default -100.0 }
              {:name "AI" :desc "AI" :type :real :default 0.0
               :link true :link-block-id "0"}
              ]
    :outputs [ :bool :bool]
    :function (wrap hl-alarm)
    }
   {:type-name "偏差高低报警"
    :inputs [ {:name "H" :desc "高限幅" :type :real :default 100.0 }
              {:name "L" :desc "低限幅" :type :real :default -100.0}
              {:name "AI1" :desc "AI1" :type :real :default 0.0
               :link true :link-block-id "0"}
              {:name "AI2" :desc "AI2" :type :real :default 0.0
               :link true :link-block-id "0"}
              ]
    :outputs [ :bool ]
    :function (wrap dev-alarm)
    }
   {:type-name "输入求和"
    :inputs [ {:name "K1" :desc "系数1" :type :real :default 0.0}
              {:name "K2" :desc "系数2" :type :real :default 0.0}
              {:name "K3" :desc "系数3" :type :real :default 0.0}
              {:name "K4" :desc "系数4" :type :real :default 0.0}
              {:name "K" :desc "总系数" :type :real :default 1.0}
              {:name "AI1" :desc "AI1" :type :real :default 0.0
               :link true :link-block-id "0"}
              {:name "AI2" :desc "AI2" :type :real :default 0.0
               :link true :link-block-id "0"}
              {:name "AI3" :desc "AI3" :type :real :default 0.0
               :link true :link-block-id "0"}
              {:name "AI4" :desc "AI4" :type :real :default 0.0
               :link true :link-block-id "0"}
              ]
    :outputs [ :real ]
    :function (fn [ctx block]
                (let [ai1 (get-block-input-value ctx block "AI1")
                      ai2 (get-block-input-value ctx block "AI2")
                      ai3 (get-block-input-value ctx block "AI3")
                      ai4 (get-block-input-value ctx block "AI4")
                      k1 (get-block-input-value ctx block "K1")
                      k2 (get-block-input-value ctx block "K2")
                      k3 (get-block-input-value ctx block "K3")
                      k4 (get-block-input-value ctx block "K4")
                      k (get-block-input-value ctx block "K")
                      v (* k (+ (* k1 ai1) (* k2 ai2) (* k3 ai3) (* k4 ai4)))]
                  (update-context ctx {(:block-id block) v})))
    }
   {:type-name "乘法器"
    :inputs [ {:name "K" :desc "总系数" :type :real :default 1.0 }
              {:name "AI1" :desc "AI1" :type :real :default 0.0
               :link false :link-block-id "0"}
              {:name "AI2" :desc "AI2" :type :real :default 0.0 
               :link false :link-block-id "0"}
              ]
    :outputs [ :real ]
    :function (fn [ctx block]
                (let [ai1 (get-block-input-value ctx block "AI1")
                      ai2 (get-block-input-value ctx block "AI2")
                      k (get-block-input-value ctx block "K")
                      v (* k ai1 ai2)]
                  (update-context ctx {(:block-id block) v})))
    }
   {:type-name "除法器"
    :inputs [ {:name "K" :desc "总系数" :type :real :default 1.0}
              {:name "AI1" :desc "AI1" :type :real :default 0.0 
               :link false :link-block-id "0"}
              {:name "AI2" :desc "AI2" :type :real :default 0.0 
               :link false :link-block-id "0"}
              ]
    :outputs [ :real ]
    :function (fn [ctx block]
                (let [ai1 (get-block-input-value ctx block "AI1")
                      ai2 (get-block-input-value ctx block "AI2")
                      k (get-block-input-value ctx block "K")
                      bid (:block-id block)]
                  (if (util/almost-zero? ai2)
                    (keep-block-value ctx bid)
                    (update-context ctx {bid (* k (/ ai1 ai2))}))))
    }
   {:type-name "输入AND"
    :inputs [ {:name "DI1" :desc "DI1" :type :bool :default true 
               :link false :link-block-id "0"}
              {:name "DI2" :desc "DI2" :type :bool :default true 
               :link false :link-block-id "0"}
              {:name "DI3" :desc "DI3" :type :bool :default true
               :link false :link-block-id "0"}
              {:name "DI4" :desc "DI4" :type :bool :default true
               :link false :link-block-id "0"}
              ]
    :outputs [ :bool ]
    :function (fn [ctx block]
                (let [di1 (get-block-input-value ctx block "DI1")
                      di2 (get-block-input-value ctx block "DI2")
                      di3 (get-block-input-value ctx block "DI3")
                      di4 (get-block-input-value ctx block "DI4")
                      v (and di1 di2 di3 di4)]
                  (update-context ctx {(:block-id block) v})))
    }
   {:type-name "输入OR"
    :inputs [ {:name "DI1" :desc "DI1" :type :bool :default false
               :link false :link-block-id "0"}
              {:name "DI2" :desc "DI2" :type :bool :default false 
               :link false :link-block-id "0"}
              {:name "DI3" :desc "DI3" :type :bool :default false
               :link false :link-block-id "0"}
              {:name "DI4" :desc "DI4" :type :bool :default false
               :link false :link-block-id "0"}
             ]
    :outputs [ :bool ]
    :function (fn [ctx block]
                (let [di1 (get-block-input-value ctx block "DI1")
                      di2 (get-block-input-value ctx block "DI2")
                      di3 (get-block-input-value ctx block "DI3")
                      di4 (get-block-input-value ctx block "DI4")
                      v (or di1 di2 di3 di4)]
                  (update-context ctx {(:block-id block) v})))
    }
   {:type-name "非"
    :inputs [{:name "DI1" :desc "DI1" :type :bool :default false 
              :link true :link-block-id "0"}]
    :outputs [ :bool ]
    :function (fn [ctx block]
                (let [di (get-block-input-value ctx block "DI1")
                      v (not di)]
                  (update-context ctx {(:block-id block) v})))
    
    }
   {:type-name "RS触发器"
    :inputs [ {:name "R" :desc "R" :type :bool :default false
               :link false :link-block-id "0"}
              {:name "S" :desc "S" :type :bool :default false
               :link false :link-block-id "0"}
             ]
    :outputs [:bool :bool]
    :function (fn [ctx block]
                (let [bid (Integer/parseInt (:block-id block))
                      s (get-block-input-value ctx block "S")
                      r (get-block-input-value ctx block "R")
                      q1 (get-current-block-value ctx (:block-id block) 1) 
                      q0 (case [s r]
                           [false false] q1
                           [true false] true
                           [false true] false
                           [true true] false)]
                  (update-context ctx { (str bid) q0 (str (inc bid)) (not q0)})))
    }
   {:type-name "定宽脉冲"
    :inputs [ {:name "DT" :desc "延时时间" :type :real :default 10.0}
              {:name "DI" :desc "DI" :type :bool :default false
               :link true :link-block-id "0"}
              ]
    :outputs [:bool :bool]
    :function (fn [ctx block]
                (let [t (:interval ctx)
                      dt (get-block-input-value ctx block "DT")
                      offset (int (/ dt t))
                      diid (get-block-input-link ctx block "DI")
                      di0 (get-block-value ctx diid 0)
                      di1 (get-block-value ctx diid 1)
                      bid (Integer/parseInt (:block-id block))
                      do1id (str bid)
                      do2id (str (inc bid))
                      last-v (get-current-block-value ctx do1id 1)
                      ]
                  (case [di1 di0]
                    [false true] (let [ctx2 (schedule-timer ctx do1id offset :reset)]
                                   (update-context ctx2 {do1id true do2id false}))
                    (update-context ctx {do1id last-v do2id (not last-v)}))))
    }
   {:type-name "延时通"
    :inputs [ {:name "DT" :desc "延时时间" :type :real :default 10.0}
              {:name "DI" :desc "DI" :type :bool :default false
               :link true :link-block-id "0"}
              ]
    :outputs [:bool :bool]
    :function (fn [ctx block]
                (let [t (:interval ctx)
                      dt (get-block-input-value ctx block "DT")
                      offset (int (/ dt t))
                      diid (get-block-input-link ctx block "DI")
                      di0 (get-block-value ctx diid 0)
                      di1 (get-block-value ctx diid 1)
                      bid (Integer/parseInt (:block-id block))
                      do1id (str bid)
                      do2id (str (inc bid))
                      last-v (get-current-block-value ctx do1id 1)
                      ctx2 (update-context ctx {do1id last-v do2id (not last-v)})
                      ]
                  (case [di1 di0]
                    [false true] (schedule-timer ctx2 do1id offset :set)
                    [true false] (update-context (cancel-timer ctx do1id)
                                                 {do1id false do2id true})
                    ctx2)))
    }
   {:type-name "延时断"
    :inputs [ {:name "DT" :desc "延时时间" :type :real :default 10.0}
              {:name "DI" :desc "DI" :type :bool :default false
               :link true :link-block-id "0"}
              ]
    :outputs [:bool :bool]
    :function (fn [ctx block]
                (let [t (:interval ctx)
                      dt (get-block-input-value ctx block "DT")
                      offset (int (/ dt t))
                      diid (get-block-input-link ctx block "DI")
                      di0 (get-block-value ctx diid 0)
                      di1 (get-block-value ctx diid 1)
                      bid (Integer/parseInt (:block-id block))
                      do1id (str bid)
                      do2id (str (inc bid))
                      last-v (get-current-block-value ctx do1id 1)
                      ctx2 (update-context ctx {do1id last-v do2id (not last-v)})
                      ]
                  (case [di1 di0]
                    [false true] (update-context ctx {do1id true do2id false})
                    [true false] (schedule-timer ctx2 do1id offset :reset)
                    ctx2)))
    }
   {:type-name "模拟量延迟"
    :inputs [ {:name "DT" :desc "延时时间" :type :real :default 1.0}
              {:name "AI" :desc "AI" :type :real :default 0.0
               :link true :link-block-id "0"}
              ]
    :outputs [ :real ]
    :function (fn [ctx block]
                (let [dt (get-block-input-value ctx block "DT")
                      t (:interval ctx)
                      offset (int (/ dt t))
                      bid (get-block-input-link ctx block "AI")
                      v (get-block-value ctx bid offset)]
                  (update-context ctx {(:block-id block) v})))
    }
   {:type-name "增强型PID"
    :inputs [ {:name "PV" :desc "被调量" :type :real :default 0.0 }
              {:name "SP" :desc "设定值" :type :real :default 0.0 }
              {:name "TR" :desc "被跟踪量" :type :real :default 0.0 }
              {:name "TS" :desc "跟踪标志" :type :bool :default false }
              {:name "KP" :desc "比例增益" :type :real :default 5.0}
              {:name "TI" :desc "积分时间" :type :real :default 100.0}
              {:name "TD" :desc "微分时间" :type :real :default 0.0}
              {:name "KD" :desc "微分增益" :type :real :default 100.0}
              {:name "FF" :desc "前馈" :type :real :default 0.0}
              {:name "KF" :desc "前馈增益" :type :real :default 100.0}
              {:name "EDB" :desc "偏差死区" :type :real :default 0.0}
              {:name "DIR" :desc "正反作用" :type :bool :default false}
              {:name "CH" :desc  "输出高限" :type :real :default 100.0}
              {:name "CL" :desc  "输出低限" :type :real :default -100.0}
              {:name "BI" :desc  "闭锁增" :type :bool :default false 
               :link false :link-block-id 0}
              {:name "BD" :desc  "闭锁减" :type :bool :default false 
               :link false :link-block-id 0}
              {:name "IDEV1" :desc "积分分离偏差低限" :type :real :default 1.0}
              {:name "IDEV2" :desc "积分分离偏差高限" :type :real :default 10.0}
              {:name "BETA" :desc "积分分离偏差系数" :type :real :default 0.4}
              ]
    :outputs [ :real :bool :bool ]
    :function epid
    }
   {:type-name "坏质量判断"
    :inputs [ {:name "AIH" :desc "AI输入上限" :type :real :default 100.0}
              {:name "AIL" :desc "AI输入下限" :type :real :default -100.0}
              {:name "RL" :desc "变化率限制" :type :real :default 100.0}
              {:name "AI" :desc "AI" :type :real :default 0.0 :link true :link-block-id "0"}
              {:name "DT" :desc "DT" :type :real :default 40.0}
              ]
    :outputs [ :real ]
    :function  (fn [ctx block]
                 (let [bid (:block-id block)
                       t (:interval ctx)
                       aih (get-block-input-value ctx block "AIH")
                       ail (get-block-input-value ctx block "AIL")
                       rl (get-block-input-value ctx block "RL")
                       iid (get-block-input-link ctx block "AI")
                       dt (get-block-input-value ctx block "DT")
                       ai0 (get-block-value ctx iid 0)
                       ai1 (get-block-value ctx iid 1)
                       dtc (dec (or (get-block-state ctx bid) 0.0))
                       r (/ (- ai0 ai1) t)
                       ]
                   (if (< (- rl) r rl)
                     (if (<= dtc 0.0)
                       (update-context (set-block-state ctx bid 0.0)
                                       {bid (not (< ail ai0 aih))})
                       (update-context (set-block-state ctx bid dtc)
                                       {bid true}))
                     (let [dtc (/ dt t)]
                       (update-context (set-block-state ctx bid dtc)
                                       {bid true}))
                     )))
    }
   {:type-name "三角波输出"
    :inputs [ {:name "T" :desc "周期" :type :real :default 100.0}]
    :outputs [ :real ]
    :function (wrap (fn [ctx block]
                      (let [t (get-clock-value-in-this-cycle ctx block)
                            T (get-block-input-value ctx block "T")
                            T14 (/ T 4.0)
                            T12 (/ T 2.0)
                            T34 (+ T12 T14)]
                        [(cond 
                          (< t T14) (/ t T14)
                          (< t T34) (- 1.0 (/ (- t T14) T14))
                          :else (- (/ (- t T34) T14) 1.0))])))
    }

   {:type-name "矩形波输出"
    :inputs [ {:name "T" :desc "周期" :type :real :default 100.0}]
    :outputs [ :real ]
    :function (wrap (fn [ctx block]
                      (let [t (get-clock-value-in-this-cycle ctx block)
                            T (get-block-input-value ctx block "T")]
                        [(if (< t (/ T 2.0))
                           0.0 
                           1.0)])))
    }
   {:type-name "三值取中"
    :inputs [ {:name "AI1" :desc "AI1" :type :real :default 0.0}
              {:name "AI2" :desc "AI2" :type :real :default 0.0}
              {:name "AI3" :desc "AI3" :type :real :default 0.0}]
    :outputs [:real]
    :function (wrap (fn [ctx block]
                      (let [ai1 (get-block-input-value ctx block "AI1")
                            ai2 (get-block-input-value ctx block "AI2")
                            ai3 (get-block-input-value ctx block "AI3")
                            v (second (sort [ai1 ai2 ai3]))]
                        [v])))
    }
   {:type-name "反馈值"
    :inputs [ {:name "AI" :desc "输入块" :type :real :default 0.0 
               :link true :link-block-id "0"}
              {:name "DEFAULT" :desc "初始值" :type :real :default 0.0}]
    :outputs [:real]
    :function (wrap (fn [ctx block]
                      (let [bid (get-block-input-link ctx block "AI")
                            v (get-block-value ctx bid 0)]
                        [v])))
    }
   {:type-name "传递函数"
    :inputs [ {:name "AI" :desc "AI" :type :real :default 0.0}
              {:name "NUM" :desc "NUM" :type :vector :default [0.1]}
              {:name "DEN" :desc "DEN" :type :vector :default [1.0 0.1]}]
    :outputs [:real]
    :function transfer-function
    }
   {:type-name "随机数发生器"
    :inputs [ {:name "H" :desc "高限" :type :real :default 1.0 }
              {:name "L" :desc "低限" :type :real :default 0.0 }
             ]
    :outputs [:real]
    :function (wrap (fn [ctx block]
                      (let [v (rand)
                            h (get-block-input-value ctx block "H")
                            l (get-block-input-value ctx block "L")]
                        [(+ (* v (- h l)) l)])))}
   {:type-name "心跳信号"
    :inputs [ {:name "T" :desc "周期" :type :real :default 1.0 }
              {:name "EN" :desc "有效" :type :bool :default true}
              ]
    :outputs [:bool]
    :function (fn [ctx block]
                (let [bid (:block-id block)
                      en (get-block-input-value ctx block "EN")]
                  (if (and en (:running ctx))
                    (let [interval (:interval ctx)
                          t (get-block-input-value ctx block "T")
                          tick (int (/ t interval))
                          b (get-current-block-value ctx bid 1)
                          st (or (get-block-state ctx bid) 0)]
                      (if (or (zero? st) (zero? tick))
                        (let [ctx2 (set-block-state ctx bid tick)]
                          (update-context ctx2 {bid (not b)}))
                        (let [ctx2 (set-block-state ctx bid (dec st))]
                          (update-context ctx2 {bid b}))))
                    (update-context ctx {bid false}))))
    }
   ])


