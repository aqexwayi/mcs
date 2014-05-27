(ns mcs.sim
  (:require [clojure.set])
  (:require [mcs.util :as util])
  (:require [mcs.db :as db])
  (:require [mcs.dp :as dp])
  (:require [mcs.blocks :as bs])
  (:require [mcs.blockclass :as bc])
  (:require [mcs.check :as check])
  (:import [java.util.concurrent Executors TimeUnit])
  (:import [java.util Date])
  (:import [java.io IOException]))

(def ^:dynamic *debug-level* 0)

(def simulation-context
  (atom {:running false
         :db-connected? false}))

(defn get-current-block-value [block-id]
  (let [bv (first (get @simulation-context :blocks-value))]
    (get bv block-id)))

(defn get-simulation-interval []
  (:interval @simulation-context))

(defn simulation-running? []
  (:running @simulation-context))

(defn simulation-turn-on! [cfg]
  (let [m1 (dp/table2map @(:AI dp/data-point-tables))
        m2 (dp/table2map @(:DI dp/data-point-tables))
        mi (merge m1 m2)
        m3 (dp/table2map @(:AO dp/data-point-tables))
        m4 (dp/table2map @(:DO dp/data-point-tables))
        mo (util/swap-key-value (merge m3 m4))
        sc {:running true
            :clock 0
            :buffer-length 500
            :blocks @bs/blocks
            :blocks-value ()
            :blocks-state {}
            :ai-blocks (map second @(:AI dp/data-point-tables)) ;; ai block-id list
            :di-blocks (map second @(:DI dp/data-point-tables)) ;; di block-id list
            :schedule-time (System/currentTimeMillis)
            :name-to-block-table mi
            :block-to-name-table mo
            }
        sc-new (merge sc cfg)
        ]
    (reset! simulation-context sc-new)
    ))

(defn simulation-turn-off! []
  (swap! simulation-context #(assoc % :running false)))

(defn controller-working? []
  (> (count (:blocks-value @simulation-context)) 4))

(defn push-blocks [ctx m]
  (update-in ctx [:blocks-value] conj m))

(defn drop-last-blocks [ctx]
  (update-in ctx [:blocks-value] drop-last))

(defn run-clock [ctx]
  (update-in ctx [:clock] inc))

(defn execute-block [context block]
  (let [bc (bc/block-class-from-type-name (:block-type block))]
    ((get bc :function) context block)))

(defn execute-blocks [context blocks]
  (let [ctx2 (reduce execute-block context (bs/sort-by-id blocks)) 
        ctx4 (run-clock ctx2)
        ;; _ (println (first (:blocks-value ctx4)))
        ]
    (if (> (count (:blocks-value ctx4)) (:buffer-length @simulation-context))
      (drop-last-blocks ctx4)
      ctx4)))

(defn- check-dp [tag meta-table]
  (let [names (set (map first @(tag dp/data-point-tables)))
        tn (subs (str tag) 1)
        meta-names (set (map :_id (filter #(= (:type %) tn) meta-table)))]
    (if (clojure.set/subset? names meta-names)
      true
      (do 
        (let [n (clojure.set/difference names meta-names)
              s (str "数据点" (seq n) "不在META表(" tn ")中！" )]
          (db/write-log! s)
          (reset! util/system-exception s))
        false))))

(defn check-meta-table []
  (let [mt (db/read-meta)]
    (and (check-dp :AI mt)
         (check-dp :DI mt)
         (check-dp :AO mt)
         (check-dp :DO mt))))

(defn one-step []
  (let [t0 (System/currentTimeMillis)
        d1 (db/read!)
        t1 (System/currentTimeMillis)
        d2 (util/map-key-from-keyword-to-string d1)
        mi (:name-to-block-table @simulation-context)
        d3 (util/map-key-by-map d2 mi)]
    (if (< (count d3) (count mi))
      (do 
        (let [ps (clojure.set/difference (set (keys mi)) (set (keys d2)))
              s (str "不能从数据库中读取变量" (first (seq ps)) "!")]
          (db/write-log! s)
          (reset! util/system-exception s))
        false)
      (let [ctx1 @simulation-context
            ctx2 (push-blocks ctx1 d3)
            ctx3 (execute-blocks ctx2 @bs/blocks)
            d4 (first (:blocks-value ctx3))
            mo (:block-to-name-table @simulation-context)
            d5 (util/map-key-by-map d4 mo)
            d6 (util/map-key-from-string-to-keyword d5)
            ]
        (reset! simulation-context ctx3)
        (doseq [[bid bv] d4] (dp/update-value! bid bv))
        (if (controller-working?)
          (let [t2 (System/currentTimeMillis)
                tw (Date.)
                ;;_ (println "time=" (- t2 t0))
                ]
            (db/write-data-with-time! tw (merge d1 d6))
            (if (> *debug-level* 0)
              (let [t3 (System/currentTimeMillis)]
                (db/write-blocks! tw d4)))))
        true))))

(defn simulate [exception-handler]
  (try
    (binding [*debug-level* 1]
      (while true
        (if (simulation-running?)
          (do
            (if (:db-connected? @simulation-context)
              (let [ctx @simulation-context
                    tc (System/currentTimeMillis)
                    t0 (:schedule-time ctx)
                    interval (int (* (:interval ctx) 1000))]
                (if (> tc t0)
                  (if (or (and (empty? (:ai ctx)) (empty? (:di ctx)))
                          ;;(db/data-ready?)
                          true)
                    (let [dt (* (inc (int (/ (- tc t0) interval))) interval)
                          t1 (+ t0 dt)]
                      (if (one-step)
                        (do
                          (swap! simulation-context #(assoc % :schedule-time t1))
                          (Thread/sleep 50))
                        (do
                          (simulation-turn-off!)
                          (Thread/sleep 200))))
                    (Thread/sleep 50))
                  (Thread/sleep 50)))
              (if (db/connect! @simulation-context)
                (do
                  (swap! simulation-context #(assoc % :db-connected? true))
                  (db/write-project! [{:host "127.0.0.1"
                                       :port 27017
                                       :db-name "scada"}
                                      @bs/blocks 
                                      @(:AI dp/data-point-tables)
                                      @(:AO dp/data-point-tables)
                                      @(:DI dp/data-point-tables)
                                      @(:DO dp/data-point-tables)])
                  (if (not (check-meta-table))
                    (simulation-turn-off!)))
                (do 
                  (reset! util/system-exception (str "无法连接数据库！"))
                  (simulation-turn-off!)))))
          (do
            (if (:db-connected? @simulation-context)
              (do
                (swap! simulation-context #(assoc % :db-connected? false))
                (db/disconnect!)))
            (Thread/sleep 200)))))
    (catch Exception e
      (do
        (.printStackTrace e)
        (exception-handler nil)))))


