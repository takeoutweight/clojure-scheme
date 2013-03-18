(ns cljscm.ns-test.foo)

(defn baz [] 123)

(def kw ::foo)

(assert (= (str kw) ":cljscm.ns-test.foo/foo"))
