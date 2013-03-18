(ns cljscm.macro-test
  (:refer-clojure :exclude [==])
  (:use-macros [cljscm.macro-test.macros :only [==]]))

(defn test-macros []
  (assert (= (== 1 1) 2)))