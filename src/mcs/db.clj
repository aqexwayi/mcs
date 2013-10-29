(ns mcs.db
  (:require [mcs.util :as util])
  (:require [monger core collection util query])
  (:use [monger operators])
  (:import [com.mongodb WriteConcern])
  (:import [java.util Date]))

(defn get-meta-table []
  (monger.collection/find-maps "META" {}))

(defn connect! [db-config]
  (try
    (monger.core/connect! db-config)
    (monger.core/use-db! (:db-name db-config))
    true
    (catch Exception e false)))

(defn disconnect! []
  (monger.core/disconnect!))

(defn interpolate-by-time [[t1 v1] [t2 v2] t]
  (let [x1 (double (.getTime t1))
        x2 (double (.getTime t2))
        x (double (.getTime t))
        ty (type v1)]
    (cond
     (= ty java.lang.String) (let [y1 (Double/parseDouble v1)
                                   y2 (Double/parseDouble v2)]
                               (util/interpolate [x1 y1] [x2 y2] x))
     (= ty java.lang.Long) (let [y1 (double v1)
                                 y2 (double v2)]
                             (util/interpolate [x1 y1] [x2 y2] x))
     (= ty java.lang.Integer) (let [y1 (double v1)
                                    y2 (double v2)]
                                (util/interpolate [x1 v1] [x2 v2] x))
     (= ty java.lang.Double) (util/interpolate [x1 v1] [x2 v2] x)
     :else v1 ;; java.util.Date / java.lang.Boolean
     )))

(defn read! []
  (let [t (Date.)
        cn "DATAIN"
        t1 (Date. (- (.getTime t) 60000))
        t2 (Date. (+ (.getTime t) 60000))
        ds1 (monger.collection/find-maps cn {:_id {$gt t1 $lte t}})
        ds2 (monger.collection/find-maps cn {:_id {$gte t $lt t2}})
        d1 (last ds1)
        d2 (first ds2)
        d1-t (:_id d1)
        d2-t (:_id d2)
        ;;_ (println "READ:" d1)
        d1-v (dissoc d1 :_id)
        d2-v (dissoc d2 :_id)]
    (if (nil? d1)
      {} 
      (if (nil? d2)
        d1-v
        d1-v
        ))))

(comment (merge-with (fn [v1 v2]
                       (interpolate-by-time [d1-t v1] [d2-t v2] t))
                     d1-v d2-v))

(defn write-data-with-time! [date data]
  (let [d (merge {:_id date} data)]
    (do
      ;; (println "WRITE:" d)
      (monger.collection/insert "DATAOUT" d))))

(defn write! [data]
  (write-data-with-time! (Date.) data))

(defn write-debug-info-with-time! [date data]
  (let [d (merge {:_id date} data)]
    (monger.collection/insert "DEBUGINFO" d)))
