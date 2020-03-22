(ns tap.component.aerator
  (:require [scad-clj.model :as m]
            [tap.constant :as const]
            [tap.math :as mth]))

(defn aerator-holes []
  (->> (range const/aerator-holes-number)
       (map (fn [hole-number]
              (->> (m/cylinder 1 (* 1.1 const/segment-face-cleaner-thickness))
                   (m/translate [0 3 0])
                   (m/rotate (* hole-number (/ const/twice-pi const/aerator-holes-number)) [0 0 1])
                   (m/rotate const/half-pi [1 0 0]))))
       (apply m/union)
       (doall)))

(defn component [pxs pys xs ys ts]
  (let [x (last xs)
        y (last ys)
        t (last ts)
        segment-face-angle (mth/tangent-angle pxs pys)
        ang (segment-face-angle t)]
    (m/difference (->> (m/cylinder const/segment-hole-radius const/segment-face-cleaner-thickness)
                       (m/rotate const/half-pi [1 0 0])
                       (m/rotate ang [0 0 1])
                       (m/translate [x y 0]))
                  (->> (aerator-holes)
                       (m/rotate ang [0 0 1])
                       (m/translate [x y 0])))))