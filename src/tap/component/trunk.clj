(ns tap.component.trunk
  (:require [scad-clj.model :as m]
            [tap.math :as mth]
            [tap.constant :as const]))

(defn segment-face [x y face-angle r]
  (->> (m/cylinder r const/segment-face-thickness)
       (m/rotate const/half-pi [1 0 0])
       (m/rotate face-angle [0 0 1])
       (m/translate [x y 0])))

(defn segment-face-hole-cleaner [x y face-angle r]
  (->> (m/cylinder r const/segment-face-cleaner-thickness)
       (m/rotate const/half-pi [1 0 0])
       (m/rotate face-angle [0 0 1])
       (m/translate [x y 0])))

(defn pipe-segment-face-holes-cleaner [[x1 x2] [y1 y2] [alpha1 alpha2] r]
  (m/union (segment-face-hole-cleaner x1 y1 alpha1 r)
           (segment-face-hole-cleaner x2 y2 alpha2 r)))

;;;

(defmulti pipe-segment-ingot-outer (fn [segment-type & _] segment-type))

(defmethod pipe-segment-ingot-outer :rand-radius [_ _ [x1 x2] [y1 y2] [alpha1 alpha2] r]
  (m/hull (segment-face x1 y1 (+ alpha1 (* m/pi (rand) 0.001)) (+ r (* (rand) 3)))
          (segment-face x2 y2 alpha2 (+ r (* (rand) 3)))))

(defmethod pipe-segment-ingot-outer :smooth [_ _ [x1 x2] [y1 y2] [alpha1 alpha2] r]
  (m/hull (segment-face x1 y1 alpha1 r)
          (segment-face x2 y2 alpha2 r)))

(defmethod pipe-segment-ingot-outer :rand-radius-smooth-tapering [_ [t1 t2] [x1 x2] [y1 y2] [alpha1 alpha2] r]
  (let [alpha1 (+ alpha1 (* m/pi (rand) 0.001))
        r1 (-> (+ r (* (- 1 t1) 4.0))
               (+ (* (rand) 3)))
        r2 (-> (+ r (* (- 1 t2) 4.0))
               (+ (* (rand) 3)))]
    (m/hull (segment-face x1 y1 alpha1 r1)
            (segment-face x2 y2 alpha2 r2))))

(defmethod pipe-segment-ingot-outer :smooth-tapering [_ [t1 t2] [x1 x2] [y1 y2] [alpha1 alpha2] r]
  (let [r1 (+ r (* (- 1 t1) 4.0))
        r2 (+ r (* (- 1 t2) 4.0))]
    (m/hull (segment-face x1 y1 alpha1 r1)
            (segment-face x2 y2 alpha2 r2))))

;;;

(defn pipe-segment-ingot-inner [[x1 x2] [y1 y2] [alpha1 alpha2] r]
  (m/hull (segment-face x1 y1 alpha1 r)
          (segment-face x2 y2 alpha2 r)))

(defn pipe-segment [pxs pys]
  (fn [x1x2 y1y2 t1t2]
    (let [segment-face-angle (mth/tangent-angle pxs pys)
          ang1ang2 (map segment-face-angle t1t2)]
      (m/difference (pipe-segment-ingot-outer :rand-radius-smooth-tapering t1t2 x1x2 y1y2 ang1ang2 const/segment-radius)
                    (pipe-segment-ingot-inner x1x2 y1y2 ang1ang2 const/segment-hole-radius)
                    (pipe-segment-face-holes-cleaner x1x2 y1y2 ang1ang2 const/segment-hole-radius)))))

(def coord-seq
  (partial partition 2 1))

(defn component [pxs pys xs ys ts]
  (->> (map (pipe-segment pxs pys)
            (coord-seq xs)
            (coord-seq ys)
            (coord-seq ts))
       (apply m/union)
       (doall)))
