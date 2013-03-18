(ns cljscm.macro-test.macros
  (:refer-clojure :exclude [==]))

(defmacro == [a b]
  `(+ ~a ~b))