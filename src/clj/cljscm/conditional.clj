(ns cljscm.conditional)
; "The VM that is evaluating the code."
(defonce ^:dynamic *current-platform*
  :jvm) ;:gambit
;  "any produced forms will evalled by the *target-platform* VM (but our macro-expansion is not necessarily being executed in this VM)"
(defonce ^:dynamic *target-platform*
  :jvm) ;:gambit
(defmacro platform-case
  "emits forms depending on *target-platform*"
  [& forms]
  (let [[forms else] (if (odd? (count forms))
                       [(butlast forms) (last forms)]
                       [forms nil])]
    ((apply hash-map forms) *target-platform* else)))