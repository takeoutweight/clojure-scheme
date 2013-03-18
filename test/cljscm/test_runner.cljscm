(ns test-runner
  (:require [cljscm.core-test :as core-test]
            [cljscm.reader-test :as reader-test]
            [cljscm.binding-test :as binding-test]
            [cljscm.ns-test :as ns-test]
            [clojure.string-test :as string-test]
            [clojure.data-test :as data-test]
            [cljscm.macro-test :as macro-test]
            [cljscm.letfn-test :as letfn-test]
            [foo.ns-shadow-test :as ns-shadow-test]
            [cljscm.top-level :as top-level]))

(set! *print-fn* js/print)

(core-test/test-stuff)
(reader-test/test-reader)
(string-test/test-string)
(data-test/test-data)
(binding-test/test-binding)
(ns-test/test-ns)
(macro-test/test-macros)
(letfn-test/test-letfn)
(ns-shadow-test/test-shadow)
(top-level/test)

(println "Tests completed without exception")


