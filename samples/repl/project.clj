(defproject ca.takeoutweight/cljscm-sample-repl "0.1.0-SNAPSHOT"
  :description ""
  :profiles {:dev {:plugins [[lein-swank "1.4.5"]
                             [lein-pprint "1.0.0"]]}}
  :dependencies [[org.clojure/clojure "1.5.0"]
                 [ca.takeoutweight/clojure-scheme "0.1.0-SNAPSHOT"]
                 [org.apache.commons/commons-io "1.3.2"]])
