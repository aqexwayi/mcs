(ns mcs.blockclass
  (:require [mcs.util :as util])
  (:require [mcs.rk4 :as rk4])
  (:require [mcs.tf :as tf])
  (:require [mcs.lm :as lm]))

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
      nil)))

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
;; (1) try to get value of current simulation block.
;; (2) input block with larger block id.
(defn get-block-value- [ctx block-id offset]
  (if (block-value-computed? ctx block-id) 
    (get (get-blocks-before ctx offset) block-id)
    (get (get-blocks-before ctx (inc offset)) block-id)))

(defn get-block-value 
  "get value of block in current clock tick, 
   if this block is not computed yet , use vaule of last clock tick"
  ([ctx block-id offset]
     (let [v (get-block-value- ctx block-id offset)]
       (if (nil? v)
         (let [v (get-block-default-value ctx block-id)]
           (if (nil? v)
             (if (some #{block-id} (:di-blocks ctx))
               false
               (if (some #{block-id} (:ai-blocks ctx))
                 0.0
                 nil))
             v))
         v)))
  ([ctx block-id offset default-value]
     (let [v (get-block-value- ctx block-id offset)]
       (if (nil? v)
         default-value
         v))))

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
      (let [v (get para :value)]
        (if (nil? v)
          (get para :default)
          v)))
    (let [bc (block-class-from-type-name (:block-type block))
          paras (get bc :inputs)
          para (find-parameter-by-name paras name)
          default-v (get para :default)]
      default-v)))

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

(defn differential2 [ctx block]
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
        v (util/limit-value (/ (+ c0 c1) 2.0) l h)
        ]
    (update-context ctx2 {bid v})))

(defn parse-num-den [num den]
  (let [n (dec (count den))
        num+ (reverse (take (inc n) (concat (reverse num) (repeat 0))))
        f #(/ % (double (first den)))
        b (map f num+) 
        a (map f den)  ;; a0 == 1.0
        ]
    [b a n]))

(defn transfer-function [ctx block]
  (let [bid (:block-id block)
        num (get-block-input-value ctx block "NUM")
        den (get-block-input-value ctx block "DEN")
        [b a n] (parse-num-den num den)
        M (or (get-block-state ctx bid)
              {:X (repeat n 0.0) :Y 0.0 :num b :den a})
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
             integ (/ (- tr (* Kf ff) (* Kp err)) Kp)
             ctx2 (set-block-state ctx bid {"ERR" err
                                            "I" integ 
                                            "D" 0.0
                                            "OUTPUT" tr})
             ;; _ (println "tr=" tr ",ff=" ff "err=" err "integ=" integ)
             ]
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

(defn left-shift [v]
  (vec (concat (rest v) (list (last v)))))

(defn right-shift [v]
  (vec (cons (first v) v)))

(defn adjust-Y0 [v pv correction-factor]
  (let [e (- pv (first v))]
    (mapv + v (repeat (* correction-factor e)))))

(defn compute-step-reponse [num den n dt l]
  (let [M0 {:X (repeat n 0.0) :Y 0.0 :num num :den den}
        f #(tf/step % 1.0 dt)]
    (take l (rest (map #(get % :Y 0.0) (iterate f M0))))))

(defn build-A [a N L]
  (let [a+ (concat (repeat (dec L) 0.0) a)
        alist (take N (iterate rest a+)) ]
    (mapv #(vec (reverse (take L %))) alist)))

(defn dmc [ctx block]
  (let [dt (:interval ctx)
        bid (:block-id block)
        L (get-block-input-value ctx block "L")
        N (get-block-input-value ctx block "N")
        lamda (get-block-input-value ctx block "LAMDA")
        q (get-block-input-value ctx block "Q") 
        delta (get-block-input-value ctx block "DELTA")
        tau (int (get-block-input-value ctx block "TAU"))
        cf (get-block-input-value ctx block "CF")
        sp (get-block-input-value ctx block "SP")
        pv (get-block-input-value ctx block "PV")
        num (get-block-input-value ctx block "NUM")
        den (get-block-input-value ctx block "DEN")
        [num+ den+ n] (parse-num-den num den)
        sr0 (compute-step-reponse num+ den+ n dt N)
        sr (take N (vec (concat (repeat tau 0.0) sr0)))
        st (get-block-state ctx bid)
        Y0 (get st :Y0 (repeat N pv))
        dU (get st :dU (repeat L 0.0))
        Y0+ (adjust-Y0 Y0 pv cf)
        A (build-A sr N L)
        f (fn [du a y0]
            (+ (util/dot-product a du) y0 (- sp)))  
        df (fn [du a y0] 
             a) 
        dU-new (lm/solve A
                         Y0+
                         {:max-iter-count 10 :lamda lamda :delta delta}
                         (left-shift dU)
                         f
                         df)
        du0 (first dU-new)
        Y0-new (mapv + (left-shift Y0+) (mapv * (repeat N du0) sr))
        ctx2 (set-block-state ctx bid {:Y0 Y0-new 
                                       :dU dU-new})
        u (get-current-block-value ctx bid 1)
        v (+ u du0)
        ;; _ (println "dU0=   " du0)
        ;; _ (println "Y0-new=" (take 5 Y0-new))
        ]
    (update-context ctx2 {bid v})))

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
        sw0 (get-block-input-value ctx block "SW")
        st- (get-block-state ctx bid)
        st (if (nil? st-) 
             {:steady? true :last-switch sw0}
             st-)
        ss (:steady? st) ;; steady state
        sw1 (:last-switch st)
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

(defn wrap-math-2 [f]
  (wrap (fn [ctx block]
          (let [ai1 (get-block-input-value ctx block "AI1")
                ai2 (get-block-input-value ctx block "AI2")]
            [(f ai1 ai2)]))))

(def block-classes
  [{:type-name "函数发生器"
    :inputs [ {:name "AI" :desc "AI输入" :type :real :default 0.0 
               :link true :link-block-id "0" :mode :link}
              {:name "N" :desc "函数分段数" :type :integer
               :change-online false
               :min-value 1 :max-value 10 :default 10 :used-as-array-size true}
              {:name "X" :desc "X" :type :array 
               :element-type :real :array-size "N"
               :default 1e6 }
              {:name "Y" :desc "Y" :type :array 
               :element-type :real :array-size "N"
               :default 0.0 }
              ]
    :outputs [:real]
    :function (wrap piecewise-function)
    }
   {:type-name "模拟量设定"
    :inputs [{:name "VALUE" :desc "输出设定" :type :real
              :default 0.0}]
    :outputs [:real]
    :function (wrap output-value-parameter)
    }
   {:type-name "开关量设定"
    :inputs [{:name "VALUE" :desc "输出设定" :type :bool
              :default false}]
    :outputs [ :bool ]
    :function (wrap output-value-parameter)
    }
   {:type-name "超前滞后"
    :inputs [ {:name "LEAD" :desc "超前时间" :type :real :default 0.0
               :min-value 0.0 }
              {:name "LAG" :desc "滞后时间" :type :real :default 0.0
               :min-value 0.0 }
              {:name "K" :desc "增益" :type :real :default 1.0 }
              {:name "RATE" :desc "输出限速" :type :real :default 1.0
               :link false :link-block-id "0" }
              {:name "TS" :desc "跟踪标志" :type :bool :default false
               :link false :link-block-id "0" :change-online false
               }
              {:name "AI" :desc "AI" :type :real :default 0.0
               :link true :link-block-id "0" :mode :link}
              ]
    :outputs [ :real ]
    :function lead-lag
    } 
   {:type-name "一阶惯性"
    :inputs [ {:name "LAG" :desc "惯性时间" :type :real :default 10.0
               :min-value 0.0 }
              {:name "K" :desc "增益" :type :real :default 1.0 }
              {:name "AI" :desc "AI" :type :real :default 0.0
               :link true :link-block-id "0" :mode :link}
              ]
    :outputs [ :real ]
    :function (wrap lag)
    } 
   {:type-name "一阶惯性跟踪"
    :inputs [ {:name "LAG" :desc "惯性时间" :type :real :default 10.0 }
              {:name "K" :desc "增益" :type :real :default 1.0 }
              {:name "TS" :desc "跟踪标志" :type :bool :default false
               :link true :link-block-id 0 :change-online false}
              {:name "AI" :desc "AI" :type :real :default 0.0
               :link true :link-block-id "0" :mode :link}
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
               :link true :link-block-id "0" :mode :link}
              ]
    :outputs [ :real ]
    :function differential
    }
   {:type-name "微分2"
    :inputs [ {:name "TD" :desc "微分时间" :type :real :default 10.0 }
              {:name "KD" :desc "微分增益" :type :real :default 8.0 }
              {:name "H" :desc "输出高限" :type :real :default 100.0 }
              {:name "L" :desc "输出低限" :type :real :default -100.0 }
              {:name "AI" :desc "AI" :type :real :default 0.0
               :link true :link-block-id "0" :mode :link}
              ]
    :outputs [ :real ]
    :function differential2
    }
   {:type-name "速率限制"
    :inputs [ {:name "UV" :desc "升速率" :type :real :default 100.0
               :min-value 0.0 }
              {:name "DV" :desc "降速率" :type :real :default 100.0
               :min-value 0.0 }
              {:name "TS" :desc "跟踪标志" :type :bool :default false }
              {:name "AI" :desc "AI" :type :real :default 0.0
               :link true :link-block-id "0" :mode :link}
              ]
    :outputs [ :real ]
    :function (wrap limit-change-rate)
    }
   {:type-name "模拟量切换"
    :inputs [ {:name "R21" :desc "IN2 TO IN1速率" :type :real :default 100.0
               :min-value 0.0 }
              {:name "R12" :desc "IN1 TO IN2速率" :type :real :default 100.0 
               :min-value 0.0 }
              {:name "SW" :desc "切换开关" :type :bool :default false
               :link true :link-block-id 0 :change-online false}
              {:name "AI1" :desc "AI1" :type :real :default 0.0
               :link true :link-block-id "0" :mode :link}
              {:name "AI2" :desc "AI2" :type :real :default 0.0
               :link true :link-block-id "0" :mode :link}
              ]
    :outputs [ :real ]
    :function ai-switch
    }
   {:type-name "高低限幅"
    :inputs [ {:name "H" :desc "高限幅" :type :real :default 100.0}
              {:name "L" :desc "低限幅" :type :real :default -100.0}
              {:name "AI" :desc "AI" :type :real :default 0.0
               :link true :link-block-id "0" :mode :link}
              ]
    :outputs [ :real ]
    :function (wrap hl-limit)
    }
   {:type-name "高低限幅报警"
    :inputs [ {:name "H" :desc "高限幅" :type :real :default 100.0}
              {:name "L" :desc "低限幅" :type :real :default -100.0}
              {:name "AI" :desc "AI" :type :real :default 0.0
               :link true :link-block-id "0" :mode :link}
              ]
    :outputs [ :bool :bool]
    :function (wrap hl-alarm)
    }
   {:type-name "偏差高低报警"
    :inputs [ {:name "H" :desc "高限幅" :type :real :default 100.0 }
              {:name "L" :desc "低限幅" :type :real :default -100.0 }
              {:name "AI1" :desc "AI1" :type :real :default 0.0
               :link true :link-block-id "0" }
              {:name "AI2" :desc "AI2" :type :real :default 0.0
               :link true :link-block-id "0" }
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
               :link true :link-block-id "0" }
              {:name "AI2" :desc "AI2" :type :real :default 0.0
               :link true :link-block-id "0" }
              {:name "AI3" :desc "AI3" :type :real :default 0.0
               :link true :link-block-id "0" }
              {:name "AI4" :desc "AI4" :type :real :default 0.0
               :link true :link-block-id "0" }
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
                      v (double (* k (+ (* k1 ai1) (* k2 ai2) (* k3 ai3) (* k4 ai4))))]
                  (update-context ctx {(:block-id block) v})))
    }
   {:type-name "乘法器"
    :inputs [ {:name "K" :desc "总系数" :type :real :default 1.0 }
              {:name "AI1" :desc "AI1" :type :real :default 0.0
               :link false :link-block-id "0" }
              {:name "AI2" :desc "AI2" :type :real :default 0.0 
               :link false :link-block-id "0" }
              ]
    :outputs [ :real ]
    :function (fn [ctx block]
                (let [ai1 (get-block-input-value ctx block "AI1")
                      ai2 (get-block-input-value ctx block "AI2")
                      k (get-block-input-value ctx block "K")
                      v (double (* k ai1 ai2))]
                  (update-context ctx {(:block-id block) v})))
    }
   {:type-name "除法器"
    :inputs [ {:name "K" :desc "总系数" :type :real :default 1.0}
              {:name "AI1" :desc "AI1" :type :real :default 0.0 
               :link false :link-block-id "0" }
              {:name "AI2" :desc "AI2" :type :real :default 0.0 
               :link false :link-block-id "0" }
              ]
    :outputs [ :real ]
    :function (fn [ctx block]
                (let [ai1 (get-block-input-value ctx block "AI1")
                      ai2 (get-block-input-value ctx block "AI2")
                      k (get-block-input-value ctx block "K")
                      bid (:block-id block)]
                  (if (util/almost-zero? ai2)
                    (keep-block-value ctx bid)
                    (let [v (double (* k (/ ai1 ai2)))]
                      (update-context ctx {bid v})))))
    }
   {:type-name "输入AND"
    :inputs [ {:name "DI1" :desc "DI1" :type :bool :default true 
               :link false :link-block-id "0" }
              {:name "DI2" :desc "DI2" :type :bool :default true 
               :link false :link-block-id "0" }
              {:name "DI3" :desc "DI3" :type :bool :default true
               :link false :link-block-id "0" }
              {:name "DI4" :desc "DI4" :type :bool :default true
               :link false :link-block-id "0" }
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
               :link false :link-block-id "0" }
              {:name "DI2" :desc "DI2" :type :bool :default false 
               :link false :link-block-id "0" }
              {:name "DI3" :desc "DI3" :type :bool :default false
               :link false :link-block-id "0" }
              {:name "DI4" :desc "DI4" :type :bool :default false
               :link false :link-block-id "0" }
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
              :link true :link-block-id "0" }]
    :outputs [ :bool ]
    :function (fn [ctx block]
                (let [di (get-block-input-value ctx block "DI1")
                      v (not di)]
                  (update-context ctx {(:block-id block) v})))
    
    }
   {:type-name "RS触发器"
    :inputs [ {:name "R" :desc "R" :type :bool :default false
               :link false :link-block-id "0" }
              {:name "S" :desc "S" :type :bool :default false
               :link false :link-block-id "0" }
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
    :inputs [ {:name "DT" :desc "延时时间" :type :real :default 10.0
               :min-value 0.0 :max-value 1000.0}
              {:name "DI" :desc "DI" :type :bool :default false :mode :link 
               :link true :link-block-id "0" }
              ]
    :outputs [:bool :bool]
    :function (fn [ctx block]
                (let [t (:interval ctx)
                      dt (get-block-input-value ctx block "DT")
                      offset (int (/ dt t))
                      diid (get-block-input-link ctx block "DI")
                      di0 (get-block-value ctx diid 0)
                      di1 (get-block-value ctx diid 1 true)
                      bid (:block-id block)
                      do1id bid
                      do2id (str (inc (Integer/parseInt bid)))
                      st (get-block-state ctx bid)
                      tc (get st "DT" 0)
                      ]
                  (case [di1 di0]
                    [false true] (let [ctx2 (update-context ctx {do1id true do2id false})]
                                   (set-block-state ctx2 bid {"DT" 1}))
                    (if (== tc 0)
                      (update-context ctx {do1id false do2id true})
                      (if (>= tc offset)
                        (let [ctx2 (update-context ctx {do1id false do2id true})]
                          (set-block-state ctx2 bid {"DT" 0}))
                        (let [ctx2 (update-context ctx {do1id true do2id false})]
                          (set-block-state ctx2 bid {"DT" (inc tc)})))))))
    }
   {:type-name "延时通"
    :inputs [ {:name "DT" :desc "延时时间" :type :real :default 10.0
               :min-value 0.0 :max-value 1000.0}
              {:name "DI" :desc "DI" :type :bool :default false :mode :link
               :link true :link-block-id "0" }
              ]
    :outputs [:bool :bool]
    :function (fn [ctx block]
                (let [t (:interval ctx)
                      dt (get-block-input-value ctx block "DT")
                      offset (int (/ dt t))
                      diid (get-block-input-link ctx block "DI")
                      di0 (get-block-value ctx diid 0)
                      di1 (get-block-value ctx diid 1 false)
                      bid (:block-id block)
                      do1id bid
                      do2id (str (inc (Integer/parseInt bid)))
                      st (get-block-state ctx bid)
                      tc (get st "DT" 0)
                      ]
                  (case [di1 di0]
                    [true true] (if (>= tc offset)
                                  (update-context ctx {do1id true do2id false})
                                  (let [ctx2 (update-context ctx {do1id false do2id true})]
                                    (set-block-state ctx2 bid {"DT" (inc tc)})))
                    [false true] (let [ctx2 (update-context ctx {do1id false do2id true})]
                                   (set-block-state ctx2 bid {"DT" 1}))
                    [false false] (let [ctx2 (update-context ctx {do1id false do2id true})]
                                    (set-block-state ctx2 bid {"DT" 0}))
                    [true false] (let [ctx2 (update-context ctx {do1id false do2id true})]
                                    (set-block-state ctx2 bid {"DT" 0}))
                    )))
    }
   
   {:type-name "延时断"
    :inputs [ {:name "DT" :desc "延时时间" :type :real :default 10.0
               :min-value 0.0 :max-value 1000.0}
              {:name "DI" :desc "DI" :type :bool :default false :mode :link
               :link true :link-block-id "0" }
              ]
    :outputs [:bool :bool]
    :function (fn [ctx block]
                (let [t (:interval ctx)
                      dt (get-block-input-value ctx block "DT")
                      offset (int (/ dt t))
                      diid (get-block-input-link ctx block "DI")
                      di0 (get-block-value ctx diid 0)
                      di1 (get-block-value ctx diid 1 false)
                      bid (:block-id block)
                      do1id bid
                      do2id (str (inc (Integer/parseInt bid)))
                      st (get-block-state ctx bid)
                      tc (get st "DT" 0)
                      ]
                  (case [di1 di0]
                    [true true] (let [ctx2 (update-context ctx {do1id true do2id false})]
                                  (set-block-state ctx2 bid {"DT" 0}))
                    [true false] (let [ctx2 (update-context ctx {do1id true do2id false})]
                                   (set-block-state ctx2 bid {"DT" 1}))
                    [false true] (let [ctx2 (update-context ctx {do1id true do2id false})]
                                   (set-block-state ctx2 bid {"DT" 0}))
                    [false false] (if (== tc 0)
                                    (update-context ctx {do1id false do2id true})
                                    (if (>= tc offset)
                                      (update-context ctx {do1id false do2id true})
                                      (let [ctx2 (update-context ctx {do1id true do2id false})]
                                        (set-block-state ctx2 bid {"DT" (inc tc)}))))
                    )))
    }
   {:type-name "模拟量迟延"
    :inputs [ {:name "DT" :desc "迟延时间" :type :real :default 1.0
               :min-value 0.0 :max-value 60.0}
              {:name "AI" :desc "AI" :type :real :default 0.0 :mode :link
               :link true :link-block-id "0" }
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
    :inputs [ {:name "PV" :desc "被调量" :type :real :default 0.0}
              {:name "SP" :desc "设定值" :type :real :default 0.0}
              {:name "TR" :desc "被跟踪量" :type :real :default 0.0}
              {:name "TS" :desc "跟踪标志" :type :bool :default true}
              {:name "KP" :desc "比例增益" :type :real :default 5.0
               :min-value 0.0}
              {:name "TI" :desc "积分时间" :type :real :default 100.0
               :min-value 0.0}
              {:name "TD" :desc "微分时间" :type :real :default 0.0
               :min-value 0.0}
              {:name "KD" :desc "微分增益" :type :real :default 100.0
               :min-value 0.0}
              {:name "FF" :desc "前馈" :type :real :default 0.0}
              {:name "KF" :desc "前馈增益" :type :real :default 1.0}
              {:name "EDB" :desc "偏差死区" :type :real :default 0.0}
              {:name "DIR" :desc "正反作用" :type :bool :default false}
              {:name "CH" :desc  "输出高限" :type :real :default 100.0}
              {:name "CL" :desc  "输出低限" :type :real :default 0.0}
              {:name "BI" :desc  "闭锁增" :type :bool :default false 
               :link false :link-block-id 0 :change-online false}
              {:name "BD" :desc  "闭锁减" :type :bool :default false 
               :link false :link-block-id 0 :change-online false}
              {:name "IDEV1" :desc "积分分离偏差低限" :type :real :default 100.0}
              {:name "IDEV2" :desc "积分分离偏差高限" :type :real :default 100.0}
              {:name "BETA" :desc "积分分离偏差系数" :type :real :default 1.0}
              ]
    :outputs [ :real :bool :bool ]
    :function epid
    }
   {:type-name "坏质量判断"
    :inputs [ {:name "AIH" :desc "AI输入上限" :type :real :default 100.0}
              {:name "AIL" :desc "AI输入下限" :type :real :default -100.0}
              {:name "RL" :desc "变化率限制" :type :real :default 100.0}
              {:name "AI" :desc "AI" :type :real :default 0.0 :mode :link
               :link true :link-block-id "0" }
              {:name "DT" :desc "延时时间" :type :real :default 40.0
               :min-value 0.0 :max-value 600.0 }
              ]
    :outputs [ :bool ]
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
                       ai2 (get-block-value ctx iid 2)
                       dtc (dec (or (get-block-state ctx bid) 0.0))
                       r (/ (- ai0 ai2) (* 2.0 t))
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
    :inputs [ {:name "T" :desc "周期" :type :real :default 100.0
               :min-value 1.0 :max-value 120.0}
              ]
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
    :inputs [ {:name "T" :desc "周期" :type :real :default 100.0
               :min-value 1.0 :max-value 120.0}]
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
   {:type-name "二值选小"
    :inputs [ {:name "AI1" :desc "AI1" :type :real :default 0.0}
              {:name "AI2" :desc "AI2" :type :real :default 0.0}]
    :outputs [:real]
    :function (wrap-math-2 min)
    }
   {:type-name "二值选大"
    :inputs [ {:name "AI1" :desc "AI1" :type :real :default 0.0}
              {:name "AI2" :desc "AI2" :type :real :default 0.0}]
    :outputs [:real]
    :function (wrap-math-2 max)
    }
   {:type-name "反馈值"
    :inputs [ {:name "AI" :desc "输入块" :type :real :default 0.0 :mode :link
               :link true :link-block-id "0" }
              {:name "DEFAULT" :desc "初始值" :type :real :default 0.0}]
    :outputs [:real]
    :function (wrap (fn [ctx block]
                      (let [bid (get-block-input-link ctx block "AI")
                            v (get-block-value ctx bid 0)]
                        [v])))
    }
   {:type-name "传递函数"
    :inputs [ {:name "AI" :desc "AI" :type :real :default 0.0}
              {:name "NUM" :desc "NUM" :type :vector :default [1.0]}
              {:name "DEN" :desc "DEN" :type :vector :default [10.0 1.0]}
              ]
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
    :inputs [ {:name "T" :desc "周期" :type :real :default 1.0}
              {:name "EN" :desc "有效" :type :bool :default true}
              ]
    :outputs [:bool]
    :function (fn [ctx block]
                (let [bid (:block-id block)
                      en (get-block-input-value ctx block "EN")
                      ]
                  (if (and en (:running ctx))
                    (let [interval (:interval ctx)
                          t (get-block-input-value ctx block "T")
                          tick (int (+ (/ t interval) 0.5))
                          b (get-current-block-value ctx bid 1)
                          st (or (get-block-state ctx bid) 0)
                          new-st (dec st)]
                      (if (or (<= new-st 0) (zero? tick))
                        (let [ctx2 (set-block-state ctx bid tick)]
                          (update-context ctx2 {bid (not b)}))
                        (let [ctx2 (set-block-state ctx bid new-st)]
                          (update-context ctx2 {bid b}))))
                    (update-context ctx {bid false}))))
    }
   {:type-name "单路预测控制"
    :inputs [{:name "PV" :desc "被调量" :type :real :default 0.0
              :link true :link-block-id 0 }
             {:name "SP" :desc "设定值" :type :real :default 0.0 
              :link true :link-block-id 0 }
             {:name "NUM" :desc "NUM" :type :vector :default [1.0]}
             {:name "DEN" :desc "DEN" :type :vector :default [10.0 1.0]}
             {:name "LAMDA" :desc "正定系数" :type :real :default 0.01
              :min-value 0.0 :max-value 1.0}
             {:name "L" :desc "控制长度" :type :real :default 60
              :min-value 10 :max-value 300 :change-online false}
             {:name "N" :desc "预测长度" :type :real :default 120
              :min-value 10 :max-value 600 :change-online false}
             {:name "Q" :desc "柔化系数" :type :real :default 1.0
              :min-value 0.0 :max-value 1000.0}
             {:name "DELTA" :desc "惩罚系数" :type :real :default 0.1
              :min-value 0.0 :max-value 1000.0}
             {:name "TAU" :desc "纯延迟" :type :real :default 0.0
              :min-value 0.0 :max-value 60.0}
             {:name "CF" :desc "修正系数" :type :real :default 1.0
              :min-value 0.1 :max-value 2.0}
             ]
    :outputs [:real]
    :function dmc
    }
   {:type-name "脉冲计数器"
    :inputs [ {:name "DI" :desc "DI" :type :bool :default false :mode :link 
               :link true :link-block-id "0"} ]
    :outputs [:real]
    :function (fn [ctx block]
                (let [bid (:block-id block)
                      diid (get-block-input-link ctx block "DI")
                      di0 (get-block-value ctx diid 0)
                      di1 (get-block-value ctx diid 1 true)
                      c (get-current-block-value ctx bid 1)]
                  (case [di1 di0]
                    [false true] (update-context ctx {bid (inc c)})
                    (update-context ctx {bid c}))))
    }
   ])

