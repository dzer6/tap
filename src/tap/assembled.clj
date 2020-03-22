(ns tap.assembled
  (:gen-class)
  (:require [scad-clj.scad :as scad]
            [scad-clj.model :as m]
            [tap.component.aerator :as aerator]
            [tap.component.trunk :as trunk]
            [tap.component.base :as base]
            [tap.constant :as const]
            [tap.math :as mth]))

(defn object []
  (let [pxs (map first const/tap-pipe-bezier-params)
        pys (map second const/tap-pipe-bezier-params)
        ts (range 0 1 const/curve-parameters-sequence-step)
        xs (map (mth/bezier pxs) ts)
        ys (map (mth/bezier pys) ts)]
    (m/union (trunk/component pxs pys xs ys ts)
             (aerator/component pxs pys xs ys ts)
             (base/component))))

;;;

(defn -main [& _]
  (->> (object)
       (scad/write-scad)
       (spit "dist/tap.scad")))
