(ns tap.constant
  (:require [scad-clj.model :as m]))

(def twice-pi (* m/pi 2))
(def half-pi (/ m/pi 2))
(def segment-radius 8)
(def segment-hole-radius 6)
(def segment-face-thickness 1)
(def segment-face-cleaner-thickness 2)
(def curve-parameters-sequence-step 0.05)
(def tap-pipe-bezier-params [[0 0] [0 60] [30 60] [45 30]])
(def aerator-holes-number 7)