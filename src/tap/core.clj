(ns tap.core
  (:gen-class)
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]))

;;;

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

;;;

(def segment-radius 8)
(def segment-hole-radius 5)
(def segment-face-thickness 1)
(def segment-face-cleaner-thickness 1.2)
(def curve-parameters-sequence-step 0.05)
(def tap-pipe-bezier-params [[0 0] [0 60] [30 60] [30 30]])

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

(defn pipe-segment-face-holes-cleaner [[x1 x2] [y1 y2] [alpha1 alpha2] r]
  (union (segment-face-hole-cleaner x1 y1 alpha1 r)
         (segment-face-hole-cleaner x2 y2 alpha2 r)))

(defn pipe-segment-ingot [[x1 x2] [y1 y2] [alpha1 alpha2] r]
  (hull (segment-face x1 y1 alpha1 r)
        (segment-face x2 y2 alpha2 r)))

(defn tangent-angle [pxs pys]
  (let [x' (bezier' pxs)
        y' (bezier' pys)]
    (fn [t]
      (-> (/ (y' t)
             (non-zero (x' t)))
          (Math/atan)
          (+ (* (Math/PI) 0.5))))))

(defn pipe-segment [pxs pys]
  (fn [x1x2 y1y2 t1t2]
    (let [segment-face-angle (tangent-angle pxs pys)
          ang1ang2 (map segment-face-angle t1t2)]
      (difference (pipe-segment-ingot x1x2 y1y2 ang1ang2 segment-radius)
                  (pipe-segment-ingot x1x2 y1y2 ang1ang2 segment-hole-radius)
                  (pipe-segment-face-holes-cleaner x1x2 y1y2 ang1ang2 segment-hole-radius)))))

(def coord-seq
  (partial partition 2 1))

(defn tap-pipe-bezier [pxs pys xs ys ts]
  (->> (map (pipe-segment pxs pys)
            (coord-seq xs)
            (coord-seq ys)
            (coord-seq ts))
       (apply union)
       (doall)))

(defn tap []
  (let [pxs (map first tap-pipe-bezier-params)
        pys (map second tap-pipe-bezier-params)
        ts (range 0 1 curve-parameters-sequence-step)
        xs (map (bezier pxs) ts)
        ys (map (bezier pys) ts)]
    (tap-pipe-bezier pxs pys xs ys ts)))

;;;

(defn -main [& _]
  (spit "dist/tap.scad"
        (write-scad
          (tap))))
