(ns cljscm.ns-test
  (:refer-clojure :exclude [+])
  (:require [cljscm.ns-test.foo :refer [baz]])
  (:use [cljscm.ns-test.bar :only [quux]]))

(def + -)

(defn test-ns []
  (assert (= 4 (clojure.core/+ 2 1 1)))
  (assert (= 0 (cljscm.ns-test/+ 2 1 1)))
  (assert (= 0 (+ 2 1 1)))
  (assert (= 123 (baz)))
  (assert (= 123 (quux)))
  :ok)
