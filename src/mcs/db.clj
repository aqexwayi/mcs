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
        t- (Date. (- (.getTime t) 60000))
        ds (monger.collection/find-maps "DATAIN" {:_id {$gt t-}})
        d (last ds)
        ]
    (if (nil? d)
      {} 
      (dissoc d :_id))))

(defn data-ready? []
  (let [t (Date.)
        t- (Date. (- (.getTime t) 60000))
        ds (monger.collection/find-maps "DATAIN" {:_id {$gt t-}})
        d (last ds)]
    (if (nil? d)
      false
      (let [t0 (:id d)]
        (if (nil? t0)
          false
          (< (- (.getTime t) (.getTime t0)) 1000))))))

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

(defn write-log [log]
  (monger.collection/insert "LOG" {:_id (Date.) :log log}))

(defn write-project [prj]
  (monger.collection/insert "PROJECT" {:_id (Date.) :prj (str prj)}))

;;
;; META TABLE FORMAT
;; ({:_id "x1", :addr "40000", :max "10", :min "-10", :type "AO"})
;; If META TABLE not found , return ()
;;
(defn read-meta []
  (monger.collection/find-maps "META" {}))
