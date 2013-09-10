(ns mcs.lm
  (:require [clatrix.core :as M]))

;; J = L*N 
;; F = L*1
;; X = N*1
;; dX = N*1

(defn step [F J X lamda delta]
  (let [[L N] (M/size J) 
        Jt (M/t J)
        A (M/+ (M/* Jt J) (M/* (+ (* 2.0 delta) lamda) (M/eye N)))
        B (M/* -1.0 (M/+ (M/* Jt F) (M/* 2.0 delta X)))
        dX (M/solve A B)
        X-new (M/+ X dX)]
    X-new))

;; f: input: X[N], points-x, points-y  output: residual
;; df: input: X[N], points-x, points-y  output: partial derivative [N]
(defn solve [points-x points-y para X0 fv dfv]
  (let [mu (get para :mu 2.0)
        max-cost (get para :max-cost 1e-6)
        iter-count (get para :max-iter-count 10)
        delta (get para :delta 0.0)
        ]
    (loop [mX (M/matrix X0) 
           lamda (get para :lamda 0.01)
           i iter-count]
      (if (zero? i)
        (M/as-vec mX)
        (let [X (M/as-vec mX)
              mF (M/matrix (mapv #(fv X %1 %2) points-x points-y))
              mJ (M/matrix (mapv #(dfv X %1 %2) points-x points-y))
              mX-new (step mF mJ mX lamda delta)
              ;; _ (println X)
              ]
          (recur mX-new (/ lamda mu) (dec i)))))))

