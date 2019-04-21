(ns tap.core
  (:gen-class)
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]))

;;;

(defn pow [base exponent]
  (reduce *' (repeat exponent base)))

(defn bezier-3 [p0 p1 p2]
  (fn [t]
    (+
      (* (pow (- 1 t) 2) p0)
      (* 2 (- 1 t) t p1)
      (* (pow t 2) p2))))

(defn bezier-3' [p0 p1 p2]
  (fn [t]
    (+
      (* 2 (- 1 t) (- p1 p0))
      (* 2 t (- p2 p1)))))

(defn non-zero [v]
  (if (= 0 v) 0.0001 v))

(defn tangent-angle [[x1 y1] [x2 y2] [x3 y3]]
  (let [x' (bezier-3' x1 x2 x3)
        y' (bezier-3' y1 y2 y3)]
    (fn [t]
      (-> (/ (y' t)
             (non-zero (x' t)))
          (Math/atan)
          (+ (* (Math/PI) 0.5))))))

;;;

(def segment-radius 10)
(def segment-hole-radius 8)
(def segment-face-thickness 1)
(def segment-face-cleaner-thickness 1.5)
(def curve-parameters-sequence-step 0.05)
(def tap-pipe-bezier-params [[20 0] [30 70] [-20 40]])

;;;

(defn segment-face [x y face-angle r]
  (->> (cylinder r segment-face-thickness)
       (rotate (* (Math/PI) 0.5) [1 0 0])
       (rotate face-angle [0 0 1])
       (translate [x y 0])))

(defn segment-face-hole-cleaner [x y face-angle r]
  (->> (cylinder r segment-face-cleaner-thickness)
       (rotate (* (Math/PI) 0.5) [1 0 0])
       (rotate face-angle [0 0 1])
       (translate [x y 0])))

(defn segment-face-holes-cleaner [[x1 x2] [y1 y2] [alpha1 alpha2] r]
  (union (segment-face-hole-cleaner x1 y1 alpha1 r)
         (segment-face-hole-cleaner x2 y2 alpha2 r)))

(defn segment-ingot [[x1 x2] [y1 y2] [alpha1 alpha2] r]
  (hull (segment-face x1 y1 alpha1 r)
        (segment-face x2 y2 alpha2 r)))

(defn segment [p1 p2 p3]
  (fn [x1x2 y1y2 t1t2]
    (let [segment-face-angle (tangent-angle p1 p2 p3)
          ang1ang2 (map segment-face-angle t1t2)]
      (difference (segment-ingot x1x2 y1y2 ang1ang2 segment-radius)
                  (segment-ingot x1x2 y1y2 ang1ang2 segment-hole-radius)
                  (segment-face-holes-cleaner x1x2 y1y2 ang1ang2 segment-hole-radius)))))

(def coord-seq
  (partial partition 2 1))

(defn tap-pipe-bezier [[p1 p2 p3] xs ys ts]
  (->> (map (segment p1 p2 p3)
            (coord-seq xs)
            (coord-seq ys)
            (coord-seq ts))
       (apply union)
       (doall)))

(defn tap []
  (let [[[x1 y1] [x2 y2] [x3 y3] :as bezier-params] tap-pipe-bezier-params
        ts (range 0 1 curve-parameters-sequence-step)
        xs (map (bezier-3 x1 x2 x3) ts)
        ys (map (bezier-3 y1 y2 y3) ts)]
    (tap-pipe-bezier bezier-params xs ys ts)))

;;;

(defn -main [& _]
  (spit "dist/tap.scad"
        (write-scad
          (tap))))
