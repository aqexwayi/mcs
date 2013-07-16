(ns mcs.sim
  (:require [mcs.util :as util])
  (:require [mcs.db :as db])
  (:require [mcs.dp :as dp])
  (:require [mcs.blocks :as bs])
  (:require [mcs.blockclass :as bc])
  (:import [java.util.concurrent Executors TimeUnit])
  (:import [java.util Date]))

(def simulation-context
  (atom {:interval 1.0
         :clock 0
         :running false
         :buffer-length 300
         :blocks {}
         :blocks-value ()
         :blocks-state {}
         :ai []
         :di []
         :timers {}
         :schedule-time 0
         }))

(defn get-current-block-value [block-id]
  (let [bv (first (get @simulation-context :blocks-value))]
    (get bv block-id)))

(defn get-simulation-interval []
  (:interval @simulation-context))

(defn simulation-running? []
  (:running @simulation-context))

(defn simulation-turn-on! [cfg]
  (let [ai (map second @(:AI dp/data-point-tables))
        di (map second @(:DI dp/data-point-tables))
        interval (:interval cfg)
        ]
    (swap! simulation-context #(assoc % 
                                 :running true
                                 :interval interval
                                 :clock 0
                                 :blocks-value ()
                                 :blocks-state {}
                                 :blocks @bs/blocks
                                 :ai ai
                                 :di di
                                 :timers {}
                                 :schedule-time (System/currentTimeMillis)
                                 ))))

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
        ctx3 (bc/run-timer ctx2)
        ctx4 (run-clock ctx3)
        ;; _ (println (first (:blocks-value ctx4)))
        ]
    (if (> (count (:blocks-value ctx4)) (:buffer-length @simulation-context))
      (drop-last-blocks ctx4)
      ctx4)))

(defn one-step []
  (let [d1 (db/read!)
        d2 (util/map-key-from-keyword-to-string d1)
        m1 (dp/table2map @(:AI dp/data-point-tables))
        m2 (dp/table2map @(:DI dp/data-point-tables))
        mi (merge m1 m2)
        d3 (util/map-key-by-map d2 mi)]
    (if (< (count d3) (count mi))
      (do 
        (println "can't get AI/DI from database!")
        false)
      (let [ctx1 @simulation-context
            ctx2 (push-blocks ctx1 d3)
            ctx3 (execute-blocks ctx2 @bs/blocks)
            d4 (first (:blocks-value ctx3))
            m3 (dp/table2map @(:AO dp/data-point-tables))
            m4 (dp/table2map @(:DO dp/data-point-tables))
            mo (util/swap-key-value (merge m3 m4))
            d5 (util/map-key-by-map d4 mo)
            d6 (util/map-key-from-string-to-keyword d5)]
        (reset! simulation-context ctx3)
        (if (controller-working?)
          (db/write! (merge d1 d6)))
        true))))

(defn simulate [exception-handler]
  (try 
    (while true
      (if (simulation-running?)
        (let [ctx @simulation-context
              tc (System/currentTimeMillis)
              t0 (:schedule-time ctx)
              interval (int (* (:interval ctx) 1000))]
          (if (> tc t0)
            (let [dt (* (inc (int (/ (- tc t0) interval))) interval)
                  t1 (+ t0 dt)]
              (if (one-step)
                (do
                  (swap! simulation-context #(assoc % :schedule-time t1))
                  (Thread/sleep 50))
                (do
                  (simulation-turn-off!)
                  (Thread/sleep 250))))
            (Thread/sleep 50)))
        (Thread/sleep 250)))
    (catch Exception e (do
                         (.printStackTrace e)
                         (exception-handler nil)))))


