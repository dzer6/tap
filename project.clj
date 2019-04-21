(defproject dzer6/tap "1.0.0-SNAPSHOT"
  :description "OpenSCAD Tap Model"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [scad-clj "0.5.3"]]
  :plugins [[lein-auto "0.1.3"]]
  :main ^:skip-aot tap.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
