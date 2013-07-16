(ns mcs.tf
  (:require [mcs.rk4 :as rk4])
  )

(defn dot-product [X1 X2]
  (let [X_ (map * X1 X2)]
    (reduce + X_)))

;; M = {:X X, :Y Y, :num num, :den den}
(defn step [M u dt]
  (let [num (:num M)
        den (:den M)
        lst (rest (range (count den))) ;; [1..n]
        fs (vec (map #(fn [X] (nth X %)) (drop-last lst)))
        f (fn [X]
            (+ (* -1.0 (dot-product (reverse (rest den)) X)) 
               u))
        X+ (rk4/rk4V (conj fs f) dt (:X M))
        fk (fn [idx] ;; idx=[n..1]
             (- (nth num idx) 
                (* (nth den idx) (first num))))
        K (map fk (reverse lst))
        y (+ (dot-product K X+)
             (* (first num) u))
        ]
    (assoc M :X X+ :Y y)))
