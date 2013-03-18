; Projects compiled with :target :nodejs have this file appended.  Its
; job is to make sure cljscm.nodejs is loaded and that the *main-cli-fn*
; is called with the script's command-line arguments.
(ns cljscm.nodejscli
  (:require [cljscm.nodejs :as nodejs]))

; Call the user's main function
(apply cljscm.core/*main-cli-fn* (drop 2 (.-argv nodejs/process)))

