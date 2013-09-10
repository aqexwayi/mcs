(ns mcs.rk4)

;; f = dx/dt 
(defn rk4 [f dt x]
  (let [k1 (f x)
        k2 (f (+ x (* 0.5 dt k1)))
        k3 (f (+ x (* 0.5 dt k2)))
        k4 (f (+ x (* dt k3)))
        k (/ (+ k1 k2 k2 k3 k3 k4) 6)]
    (+ x (* k dt))))

(defmacro addV [& args]
  `(mapv + ~@args))

(defmacro subV [& args]
  `(mapv - ~@args))

(defn mulSV [s V]
  (map #(* s %) V))

(defn applyV [F X]
  (map #(% X) F))

(defn rk4V [dXdt dt X]
  (let [X1 (or X (repeat (count dXdt) 0.0))
        D1 (applyV dXdt X1)
        X2 (addV X1 (mulSV (/ dt 2.0) D1))
        D2 (applyV dXdt X2)
        X3 (addV X1 (mulSV (/ dt 2.0) D2))
        D3 (applyV dXdt X3)
        X4 (addV X1 (mulSV dt D3))
        D4 (applyV dXdt X4)
        X+ (addV X1 (mulSV (/ dt 6.0) (addV D1 D2 D2 D3 D3 D4)))
        ]
    X+))

