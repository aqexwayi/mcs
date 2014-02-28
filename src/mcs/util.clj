(ns mcs.util
  (:require [clojure.string]))

(def system-exception (atom nil))

(defn indices-of [f coll]
  (keep-indexed #(if (f %2) %1 nil) coll))

(defn first-index-of [f coll]
  (first (indices-of f coll)))

(defn map-value [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn map-key [m f]
  (into {} (for [[k v] m] [(f k) v])))

(defn swap-key-value [m]
  (into {} (for [[k v] m] [v k])))

(defn map-key-from-keyword-to-string [data]
  (map-key data #(subs (str %) 1)))
 
(defn map-key-from-string-to-keyword [data]
  (map-key data #(keyword %)))

;; (k,v) will be removed if k is not in key-map.
(defn map-key-by-map [data key-map]
  (dissoc (map-key data #(get key-map %)) nil))

(defn interpolate [[x1 y1] [x2 y2] x]
  (cond
   (<= x x1) y1
   (>= x x2) y2
   :else (+ y1 (* (- x x1) (/ (- y2 y1) (- x2 x1))))))

(defn limit-value [v l h]
  (cond
   (< v l) l
   (> v h) h
   :else v))

(defn not-zero [x]
  (if (zero? x)
    0.000001
    x))

(defn almost-zero? [x]
  (if (< -0.000001 x 0.000001)
    true
    false))

(defn almost-eq? [x1 x2]
  (almost-zero? (- x1 x2)))

(defn dead-zone [v edb]
  (let [e (if (neg? edb) (- edb) edb)]
    (if (< (- e) v e)
      0.0
      (if (neg? v)
        (+ v e)
        (- v e)))))

(defn dot-product [X1 X2]
  (let [X_ (map * X1 X2)]
    (reduce + X_)))

(defn format-value [v]
  (if (= (class v) java.lang.Double)
    (format "%.6f" v)
    (str v)))

(defn is-os-windows? []
  (let [os-name (System/getProperty "os.name")]
    (.startsWith os-name "Windows")))

(defn trim+ [s]
  (if (string? s)
    (clojure.string/trim s)
    s))

(defn double-vector? [v]
  (if (vector? v)
    (try 
      (dorun (map double v))
      true
      (catch Exception e false))
    false))

(defn postfixed-file-name [file-name postfix]
  (if (.endsWith file-name postfix)
    file-name
    (str file-name "." postfix)))
