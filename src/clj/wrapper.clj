(ns wrapper
  #_(:use [cljs.core :only scm*])
  (:require [clojure.string :as str]))

(defn valid-method?
  "rpythonic strips anything with template arguments, leaving templatted return values for us to avoid on our own."
  [meth]
  (and (not (re-find #"<" (:returns meth)))
       (not (re-find #"std::" (:returns meth)))
       (not (some #(re-find #"std::" (:type %)) (:arguments meth)))))

(def pbr (java.io.PushbackReader. (clojure.java.io/reader (clojure.java.io/file "/tmp/debug.clj"))))
(def class-specs (read pbr))
;todo Can't create ones that are argument-polymorphic -- those we need to fail on.
;[name, arity, class] -> list of methods (if more than one, we hope a multi-method can distinguish?)
;(for [[n mm] foreign-methods [arity mm1] mm [class meths] mm1] )
;(frequencies (for [[n mm] foreign-methods [arity mm1] mm [class meths] mm1 meth meths] (try (valid-method? meth) (catch Exception e :except)))) = 4398
(def foreign-methods
  (reduce (fn [mmap {:keys [name arguments classpath] :as m}]
            (update-in mmap [name (count arguments) classpath] (comp vec conj) m))
          {}
          (for [c class-specs m (:methods c) :when (valid-method? m)]
            (assoc m :classpath (:classpath c)))))

;[classpath, method, arity, implementations]
(def foreign-methods-by-class
  (reduce (fn [mmap {:keys [classpath name arguments] :as m}]
            (update-in mmap [classpath  name (count arguments)] (comp vec conj) m))
          {}
          (for [c class-specs m (:methods c) :when (valid-method? m)]
            (assoc m :classpath (:classpath c)))))

;we have 123 multimethodic methods:
;TODO: actually generate multimethods when possible
(defn multimethodic? [methmap]
  (some #(> % 1) (for [[arity fm0] methmap [classpath meths] fm0] (count meths))))

;{true 500, nil 2532} : (frequencies (for [[n mm] foreign-methods] (polymorphic? mm)))
(defn polymorphic?
  "True when there is an arity that has more than one class implementing it.
   {arity mmap} -- must defer to a protocol."
  [methmap]
  (some #(> (count %) 1) (for [[arity fm0] methmap] fm0)))

(defn protocol-name [methname]
  (symbol (str "I" methname)))

(defn protocol-methname [methname]
  (symbol (str "-" methname)))

;{:keys [name arguments returns class]}
(defn create-protocol [methname argnametypes]
  (let [protosym (protocol-name methname)
        methsym (protocol-methname methname)]
    `(defprotocol ~protosym
       (~methsym ~@(map #(vec (map symbol %)) argnametypes)))))

;TODO: define methods that just call the protocols.
(def foreign-protocols
  (for [[nm fm0] foreign-methods :when (polymorphic? fm0)]
    (let [argnametypes (for [[arity fm1] fm0]
                         (first (for [[classpath meths] fm1]
                                  (map :name (:arguments (first meths))))))]
      (create-protocol nm argnametypes))))

(defn c-stub-name [classpath method]
  (str (str/replace classpath #"::" "_") "_" method))

(defn classpath-to-clojurename [classpath]
  (let [ss (str/split classpath #"::")]
    (symbol (first ss) (str/join "$" (rest ss)))))

;TODO: proper size_t wrapper.
(defn fundamental-type-alias
  "for the 'fundamental' types
unsigned long long int -> unsigned long long,
for now: size_t -> unsigned long"
  [c-type-string]
  (if (re-find #"std::" c-type-string)    
    (throw (Exception. (str "--ERROR: un-massaged std type argument--" c-type-string)))
    (let [long? (re-find  #"long" c-type-string)
          int? (re-find #" int$" c-type-string )
          cstr (cond
                 (and long? int?) (str/replace c-type-string #" int$" "")
                 (= "size_t" c-type-string) "unsigned long"
                 :else c-type-string)]
      (symbol (str/replace cstr #" " "-")))))

(defn type-alias
  "takes a Class to be given a c-define-type stub"
  [classpath]
  (if (re-find #"std::" classpath)
    (throw (Exception. (str "--ERROR: un-massaged std type--" classpath)))
    (get alias-map classpath
         (symbol (str (classpath-to-clojurename classpath) "-foreign-ptr")))))


(defn implement-method [classpath method])

;TODO: file-per-namespace gather.
;TODO: tags is a list of the known SUB classes (not the parents!) -- types whose primary tag can inhabit "us"
(defn create-class [{:keys [classname classpath] :as class-spec}]
  (let [clj-name (classpath-to-clojurename classpath)
        ta (type-alias classpath)]
    `(do (~'scm* {} (~'c-define-type ~ta (~'pointer (~'type ~classpath) ~clj-name)))
         (deftype* ~clj-name [] :no-constructor)
         (~'scm* {} (~'table-set! cljs.core/foreign-tags (quote ~clj-name) ~clj-name)))))

(def foreign-types
  (for [c class-specs]
    (create-class c)))

(defn clj-args
  "names for arguments"
  [c]
  (let [reg-args (vec (map (comp symbol :name) (:arguments c)))
        self-arg (first (filter (complement (set reg-args))
                                (map #(symbol (str "self" %)) (concat [nil] (range)))))]
    (vec (concat [self-arg] reg-args))))


;TODO: c-types -> scheme aliases.
(defn c-lambda
  ":returns assumed to be not-namespaced due to rpythonic. strip off the classpath's namespace, use that, and hope for the best."
  [c]
  `(~'scm-str* ~(cljs.compiler/strict-str
                 "(c-lambda "(cons (type-alias (:classpath c))
                                   (map #(if (:fundamental %)
                                           (fundamental-type-alias (:type %))
                                           (type-alias (:type %)))
                                        (:arguments c)))
                 " " (if (:returns_fundamental c)
                       (fundamental-type-alias (:returns c))
                       (type-alias (str (first (str/split (:classpath c) #"::")) "::" (:returns c))))
                 " \""(c-stub-name (:classpath c) (:name c))"\")")))

(defn create-funtion-stub
  "can create a monomorphic stub for a fn that isn't multimethodic in any of its arities"
  [aritymap]
  (let [calls (for [[arity am0] aritymap [classpath meths] am0] (first meths))
        clojure-name (symbol (:name (first calls)))
        c-name (c-stub-name (:classpath (first calls)) (:name (first calls)))]
    (if (> (count calls) 1)
      `(defn ~clojure-name
         ~@(for [c calls]
             (let [args (clj-args c) ]
               `(~args
                 (~(c-lambda c)
                  ~@args)))))
      `(def ~clojure-name
         ~(c-lambda (first calls))))))

;we have 2909 -- 2335 direct calls, 574 polyarities.
;NOTE: this code relies on the c->scheme aliases being set up already
(def foreign-monomorphs
  (for [[nm fm0] foreign-methods :when (not (polymorphic? fm0))]
    (create-funtion-stub fm0)))

;TODO: extend-class, we need the types to define the stub's arguments, need the stubs to be called by the types
;we just need create-function-stub but w/o the def/define form -- just the proto name. factor out.
;FIXME: if one class implements two polyvarities, we need to group them or we'll clobber.
; arity->methods (belonging to one class)
;we expect only one impl per arity-- if not we're multimethodic
;meant to be unquote-spliced as it returns a list of impls.
(defn implement-protocols [method arity->impls]
  `[~(protcol-name method)
    (~(protocol-methname method)
     ~@(for [[arity impls] arity->impls]
         (do
           (when (> 1 (count impls))          
             (println (str "Warning! Multimethodic " method " ignoring all-but-the-first")))
           `(~(clj-args (first impls)) (~(c-lambda (first impls)) ~@(clj-args (first impls)))))))])

(def extensions
  (for [[classpath mm0] foreign-methods-by-class]
    `(extend-type ~(classpath-to-clojurename classpath)
       ~@(mapcat (fn [[meth mm1]] (implement-protocols meth mm1)) mm0))))


(defn write-stubs [filename]
  (spit filename "(ns Ogre)\n")
  (doall
   (for [p foreign-protocols]
     (spit filename (cljs.compiler/strict-str p "\n") :append true)))
  (doall
   (for [p foreign-types]
     (spit filename (cljs.compiler/strict-str p "\n") :append true)))
  (doall
   (for [e extensions]
     (spit filename (cljs.compiler/strict-str e "\n") :append true)))
  nil)

;TODO - a nice fn that calls the proto method with the right arity.
;TODO - generate constructors, destructors
;TODO - operator equality





