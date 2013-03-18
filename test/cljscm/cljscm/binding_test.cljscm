(ns cljscm.binding-test
  (:require [cljscm.binding-test-other-ns :as o]))

(defn test-binding []
  (binding [o/*foo* 2]
    (assert (= o/*foo* 2)))
  (assert (= o/*foo* 1)))