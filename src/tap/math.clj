(ns tap.math
  (:require [tap.constant :as const]))

(defn pow [base exponent]
  (reduce *' (repeat exponent base)))

(defn bezier [[p0 p1 p2 p3]]
  (fn [t]
    (+
      (* (pow (- 1 t) 3) p0)
      (* 3 (pow (- 1 t) 2) t p1)
      (* 3 (- 1 t) (pow t 2) p2)
      (* (pow t 3) p3))))

(defn bezier' [[p0 p1 p2 p3]]
  (fn [t]
    (+
      (* 3 (pow (- 1 t) 2) (- p1 p0))
      (* 6 (- 1 t) t (- p2 p1))
      (* 3 (pow t 2) (- p3 p2)))))

(defn non-zero [v]
  (if (= 0 v) 0.0001 v))

(defn tangent-angle [pxs pys]
  (let [x' (bezier' pxs)
        y' (bezier' pys)]
    (fn [t]
      (-> (/ (y' t)
             (non-zero (x' t)))
          (Math/atan)
          (+ const/half-pi)))))
