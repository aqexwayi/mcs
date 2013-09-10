(ns mcs.tf
  (:require [mcs.util :as util])
  (:require [mcs.rk4 :as rk4])
  )

;; M = {:X X, :Y Y, :num num, :den den}
(defn step [M u dt]
  (let [num (:num M) ;; [n]
        den (:den M) ;; [n+1]
        lst (range 1 (count den)) ;; [1..n]
        fs (vec (map #(fn [X] (nth X %)) (drop-last lst)))
        f (fn [X]
            (+ (* -1.0 (util/dot-product (reverse (rest den)) X)) 
               u))
        X+ (rk4/rk4V (conj fs f) dt (:X M))
        fk (fn [idx] ;; idx=[n..1]
             (- (nth num idx) 
                (* (nth den idx) (first num))))
        K (map fk (reverse lst))
        y (+ (util/dot-product K X+)
             (* (first num) u))
        ]
    (assoc M :X X+ :Y y)))
