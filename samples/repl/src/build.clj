(ns build
  (:require [cljscm.compiler :as comp]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(defn build-file [fln]
  (println "building " fln)
  (binding [comp/*emit-source-loc?* true]
    (comp/compile-file* (io/resource fln)
                        (->> (string/replace fln #"\.clj.*" ".scm")
                             (str "scm/")
                             (io/file)
                             (comp/mkdirs)))))

(defn copy-file [fln]
  (println "copying " fln)
  (org.apache.commons.io.FileUtils/copyURLToFile
   (io/resource fln)
   (-> (str "scm/" fln)
       (io/file)
       (comp/mkdirs))))

(defn build-repl []
  (build-file "cljscm/core.cljscm")
  (build-file "cljscm/core_macros.clj")
  (build-file "cljscm/reader.cljscm")
  (build-file "clojure/walk.cljscm")
  (build-file "cljscm/conditional.clj")
  (build-file "cljscm/analyzer.clj")
  (build-file "cljscm/compiler.clj")
  (copy-file "cljscm/polymorphic-apply.scm")
  (copy-file "cljscm/source-at.scm"))

(defn -main [] (build-repl))
