(defproject ca.takeoutweight/clojure-scheme "0.1.0-SNAPSHOT"
  :description "Clojure to Gambit Scheme Compiler"
  :source-paths ["src/clj", "src/cljscm", "test/cljscm"]
  :profiles {:dev {:plugins [[lein-swank "1.4.5"]
                             [lein-pprint "1.0.0"]]}}
  :dependencies [[org.clojure/clojure "1.5.0"]])