(defproject cljex "0.1.0-SNAPSHOT"
  :description "Clojure functions by example"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.logic "1.0.1"]
                 [org.clojure/math.combinatorics "0.2.0"]]
  :profiles {:dev {:dependencies [[speclj "3.4.3"]]}}
  :plugins [[speclj "3.4.3"]]
  :test-paths ["spec"])
