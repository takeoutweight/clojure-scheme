;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
;(in-ns 'cljscm.core)
(ns cljscm.core
  (:require [cljscm.conditional :as condc]))


(condc/platform-case
 :jvm :jvm
 :gambit :gambit)

(condc/platform-case
 :jvm (ns cljscm.core
        (:refer-clojure :exclude [-> ->> .. amap and areduce alength alias aclone assert binding bound-fn comment cond condp
                                  declare definline definterface defmacro defmethod defmulti defn defn- defonce
                                  defprotocol defrecord defstruct deftype delay destructure doseq dosync dotimes doto
                                  extend-protocol extend-type fn for future gen-class gen-interface
                                  if-let if-not import io! lazy-cat lazy-seq let letfn locking loop
                                  memfn ns or proxy proxy-super pvalues refer-clojure reify sync seq time
                                  when when-first when-let when-not while with-bindings with-in-str
                                  with-loading-context with-local-vars with-open with-out-str with-precision with-redefs
                                  satisfies? identical? true? false? nil?

                                  aget aset
                                  + - * / < <= > >= == zero? pos? neg? inc dec max min mod quot rem 
                                  bit-and bit-and-not bit-clear bit-flip bit-not bit-or bit-set
                                  bit-test bit-shift-left bit-shift-right bit-xor])
        (:require [clojure.walk]
                  [clojure.string :as string]           
                  [cljscm.analyzer :as ana]))
 :gambit (ns cljscm.core
           (:require [cljscm.analyzer :as ana])))

(condc/platform-case
 :jvm (defn get-namespaces [] ana/namespaces))

; Just assumes symbols are quoted now, won't eval in gambit.
(clojure.core/defmacro alias [alias lib]
  (condc/platform-case
   :jvm (do
          (println "; in JVM")
          (try (clojure.core/alias (second alias) (second lib)) ;real aliases are needed for reader to properly expand.
               (catch java.lang.IllegalStateException e nil))  ;realiasing is an exception; fix in analyzer's ns evaluation.
          (swap! (get-namespaces) #(assoc-in % [(.getName *ns*) :requires (second alias)] (second lib)))
          ;(swap! ana/namespaces #(assoc-in % [(.getName *ns*) :requires-macros (find-ns alias)] lib))
          (println "; added " (get-in @(get-namespaces) [(.getName *ns*) :requires])))
   :gambit (do
             (println "; in gambit")
             (swap! (get-namespaces) #(assoc-in % [ana/*cljs-ns* :requires (second alias)] (second lib)))
             (println "; firing the gambit-runtime-safe macro code.")))
  (str "aliased " (second alias) " to " (second lib)))

(condc/platform-case
 :jvm (cljscm.core/alias 'core 'clojure.core)
 :gambit (cljscm.core/alias 'core 'cljscm.core))

(def

  ^{:doc "Like defn, but the resulting function name is declared as a
  macro and will be used as a macro by the compiler when it is
  called."
    :macro true
    :arglists '([name doc-string? attr-map? [params*] body]
                  [name doc-string? attr-map? ([params*] body)+ attr-map?])
    :added "1.0"}
  defmacro (core/fn [&form &env 
                     name & args]
             (core/let [prefix (core/loop [p (list (with-meta name {:macro true}))  args args] ;
                                 (core/let [f (first args)]
                                   (if (string? f)
                                     (recur (cons f p) (next args))
                                     (if (map? f)
                                       (recur (cons f p) (next args))
                                       p))))
                        fdecl (core/loop [fd args]
                                (if (string? (first fd))
                                  (recur (next fd))
                                  (if (map? (first fd))
                                    (recur (next fd))
                                    fd)))
                        fdecl (if (vector? (first fdecl))
                                (list fdecl)
                                fdecl)
                        add-implicit-args (core/fn [fd]
                                            (core/let [args (first fd)]
                                              (cons (vec (cons '&form (cons '&env args))) (next fd))))
                        add-args (core/fn [acc ds]
                                   (if (core/nil? ds)
                                     acc
                                     (core/let [d (first ds)]
                                       (if (map? d)
                                         (conj acc d)
                                         (recur (conj acc (add-implicit-args d)) (next ds))))))
                        fdecl (core/seq (add-args [] fdecl))
                        decl (core/loop [p prefix d fdecl]
                               (if p
                                 (recur (next p) (cons (first p) d))
                                 d))]
               (concat ['do]
                       (condc/platform-case
                        :jvm (case condc/*target-platform*
                               :jvm [(cons `core/defn decl)
                                     (list '. (list 'var name) '(setMacro))
                                     (list 'var name)]
                               :gambit [(cons `defn decl)
                                        `(swap! (get-namespaces) assoc-in [(quote ~ana/*cljs-ns*) :defs (quote ~name) :macro] true)
                                        name])
                        :gambit [(cons `defn decl)
                                 `(swap! (get-namespaces) assoc-in [(quote ~ana/*cljs-ns*) :defs (quote ~name) :macro] true)
                                 name])
                       ))))

(condc/platform-case
 :jvm (. (var defmacro) (setMacro))
 :gambit (swap! (get-namespaces) assoc-in ['cljscm.core :defs 'defmacro :macro] true))

(defmacro defonce [name expr]
  (condc/platform-case
   :jvm (case condc/*target-platform*
          :jvm `(let [v# (def ~name)]
                  (when-not (.hasRoot v#)
                    (def ~name ~expr)))
          :gambit `(def ~name ~expr)); TODO
   :gambit `(def ~name ~expr)))

(defmacro ->
  "Threads the expr through the forms. Inserts x as the
  second item in the first form, making a list of it if it is not a
  list already. If there are more forms, inserts the first form as the
  second item in second form, etc."
  {:added "1.0"}
  ([x] x)
  ([x form] (if (seq? form)
              (with-meta `(~(first form) ~x ~@(next form)) (meta form))
              (list form x)))
  ([x form & more] `(-> (-> ~x ~form) ~@more)))

(defmacro ->>
  "Threads the expr through the forms. Inserts x as the
  last item in the first form, making a list of it if it is not a
  list already. If there are more forms, inserts the first form as the
  last item in second form, etc."
  {:added "1.1"} 
  ([x form] (if (seq? form)
              (with-meta `(~(first form) ~@(next form)  ~x) (meta form))
              (list form x)))
  ([x form & more] `(->> (->> ~x ~form) ~@more)))

(defmacro ..
  "form => fieldName-symbol or (instanceMethodName-symbol args*)

  Expands into a member access (.) of the first member on the first
  argument, followed by the next member on the result, etc. For
  instance:

  (.. System (getProperties) (get \"os.name\"))

  expands to:

  (. (. System (getProperties)) (get \"os.name\"))

  but is easier to write, read, and understand."
  {:added "1.0"}
  ([x form] `(. ~x ~form))
  ([x form & more] `(.. (. ~x ~form) ~@more)))

(defmacro and
  "Evaluates exprs one at a time, from left to right. If a form
  returns logical false (nil or false), and returns that value and
  doesn't evaluate any of the other expressions, otherwise it returns
  the value of the last expr. (and) returns true."
  {:added "1.0"}
  ([] true)
  ([x] x)
  ([x & next]
   `(let [and# ~x]
      (if and# (and ~@next) and#))))

(defmacro assert
  "Evaluates expr and throws an exception if it does not evaluate to
  logical true."
  {:added "1.0"}
  ([x]
     (when *assert*
       `(when-not ~x
          (throw (new AssertionError (str "Assert failed: " (pr-str '~x)))))))
  ([x message]
     (when *assert*
       `(when-not ~x
          (throw (new AssertionError (str "Assert failed: " ~message "\n" (pr-str '~x))))))))

(defmacro comment
  "Ignores body, yields nil"
  {:added "1.0"}
  [& body])

(defmacro cond
  "Takes a set of test/expr pairs. It evaluates each test one at a
  time.  If a test returns logical true, cond evaluates and returns
  the value of the corresponding expr and doesn't evaluate any of the
  other tests or exprs. (cond) returns nil."
  {:added "1.0"}
  [& clauses]
    (when clauses
      (list 'if (first clauses)
            (if (next clauses)
                (second clauses)
                (throw (IllegalArgumentException.
                         "cond requires an even number of forms")))
            (cons 'clojure.core/cond (next (next clauses))))))

(defmacro declare
  "defs the supplied var names with no bindings, useful for making forward declarations."
  {:added "1.0"}
  [& names] `(do ~@(map #(list 'def (vary-meta % assoc :declared true)) names)))

(declare emit-extend-protocol)

(def ^{:private true :dynamic true}
  assert-valid-fdecl (fn [fdecl]))

(condc/platform-case
 :jvm (def sigs #'core/sigs)
 :gambit
 (def
   ^{:private true}
   sigs
   (fn [fdecl]
     (assert-valid-fdecl fdecl)
     (let [asig
           (fn [fdecl]
             (let [arglist (first fdecl)
;elide implicit macro args
                   arglist (if (= '&form (first arglist))
                             (subvec arglist 2 (count arglist))
                             arglist)
                   body (next fdecl)]
               (if (map? (first body))
                 (if (next body)
                   (with-meta arglist (conj (if (meta arglist) (meta arglist) {}) (first body)))
                   arglist)
                 arglist)))]
       (if (seq? (first fdecl))
         (loop [ret [] fdecls fdecl]
           (if fdecls
             (recur (conj ret (asig (first fdecls))) (next fdecls))
             (seq ret)))
         (list (asig fdecl)))))))

(def 

 ^{:doc "Same as (def name (fn [params* ] exprs*)) or (def
    name (fn ([params* ] exprs*)+)) with any doc-string or attrs added
    to the var metadata. prepost-map defines a map with optional keys
    :pre and :post that contain collections of pre or post conditions."
   :arglists '([name doc-string? attr-map? [params*] prepost-map? body]
                [name doc-string? attr-map? ([params*] prepost-map? body)+ attr-map?])
   :added "1.0"}
 defn (core/fn defn [&form &env name & fdecl]
        ;; Note: Cannot delegate this check to def because of the call to (with-meta name ..)
        (if (condc/platform-case
             :jvm (instance? clojure.lang.Symbol name)
             :gambit (symbol? name))
          nil
          (throw (condc/platform-case
                  :jvm (IllegalArgumentException. "First argument to defn must be a symbol")
                  :gambit "First argument to defn must be a symbol")))
        (let [m (if (string? (first fdecl))
                  {:doc (first fdecl)}
                  {})
              fdecl (if (string? (first fdecl))
                      (next fdecl)
                      fdecl)
              m (if (map? (first fdecl))
                  (conj m (first fdecl))
                  m)
              fdecl (if (map? (first fdecl))
                      (next fdecl)
                      fdecl)
              fdecl (if (vector? (first fdecl))
                      (list fdecl)
                      fdecl)
              m (if (map? (last fdecl))
                  (conj m (last fdecl))
                  m)
              fdecl (if (map? (last fdecl))
                      (butlast fdecl)
                      fdecl)
              m (conj {:arglists (list 'quote (sigs fdecl))} m)
              m (let [inline (:inline m)
                      ifn (first inline)
                      iname (second inline)]
                  ;; same as: (if (and (= 'fn ifn) (not (symbol? iname))) ...)
                  (if false #_(if (clojure.lang.Util/equiv 'fn ifn) ;TODO
                        (if (instance? clojure.lang.Symbol iname) false true))
                    ;; inserts the same fn name to the inline fn if it does not have one
                    :TODO #_(assoc m :inline (cons ifn (cons (clojure.lang.Symbol/intern (.concat (.getName ^clojure.lang.Symbol name) "__inliner"))
                                                     (next inline))))
                    m))
              m (conj (if (meta name) (meta name) {}) m)]
          (list 'def (with-meta name m)
                ;;todo - restore propagation of fn name
                ;;must figure out how to convey primitive hints to self calls first
                (cons `fn fdecl) ))))

(condc/platform-case
 :jvm (. (var defn) (setMacro))
 :gambit (swap! (get-namespaces) assoc-in ['cljscm.core :defs 'defn :macro] true))

(defmacro defn-
  "same as defn, yielding non-public def"
  {:added "1.0"}
  [name & decls]
  (list* `defn (with-meta name (assoc (meta name) :private true)) decls))

(defmacro doto
  "Evaluates x then calls all of the methods and functions with the
  value of x supplied at the front of the given arguments.  The forms
  are evaluated in order.  Returns x.

  (doto (new java.util.HashMap) (.put \"a\" 1) (.put \"b\" 2))"
  {:added "1.0"}
  [x & forms]
    (let [gx (gensym)]
      `(let [~gx ~x]
         ~@(map (core/fn [f]
                  (if (seq? f)
                    `(~(first f) ~gx ~@(next f))
                    `(~f ~gx)))
                forms)
         ~gx)))

(condc/platform-case :jvm (def emit-extend-protocol #'core/emit-extend-protocol))
(defmacro extend-protocol 
  "Useful when you want to provide several implementations of the same
  protocol all at once. Takes a single protocol and the implementation
  of that protocol for one or more types. Expands into calls to
  extend-type:

  (extend-protocol Protocol
    AType
      (foo [x] ...)
      (bar [x y] ...)
    BType
      (foo [x] ...)
      (bar [x y] ...)
    AClass
      (foo [x] ...)
      (bar [x y] ...)
    nil
      (foo [x] ...)
      (bar [x y] ...))

  expands into:

  (do
   (clojure.core/extend-type AType Protocol 
     (foo [x] ...) 
     (bar [x y] ...))
   (clojure.core/extend-type BType Protocol 
     (foo [x] ...) 
     (bar [x y] ...))
   (clojure.core/extend-type AClass Protocol 
     (foo [x] ...) 
     (bar [x y] ...))
   (clojure.core/extend-type nil Protocol 
     (foo [x] ...) 
     (bar [x y] ...)))"
  {:added "1.2"}

  [p & specs]
  (emit-extend-protocol p specs))

(condc/platform-case :jvm (def reduce1 #'core/reduce1))

(defmacro ^{:private true} assert-args [fnname & pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                  ~(core/str fnname " requires " (second pairs)))))
     ~(core/let [more (nnext pairs)]
        (when more
          (list* `assert-args fnname more)))))

(defmacro for
  "List comprehension. Takes a vector of one or more
   binding-form/collection-expr pairs, each followed by zero or more
   modifiers, and yields a lazy sequence of evaluations of expr.
   Collections are iterated in a nested fashion, rightmost fastest,
   and nested coll-exprs can refer to bindings created in prior
   binding-forms.  Supported modifiers are: :let [binding-form expr ...],
   :while test, :when test.

  (take 100 (for [x (range 100000000) y (range 1000000) :while (< y x)] [x y]))"
  {:added "1.0"}
  [seq-exprs body-expr]
  (assert-args
     (vector? seq-exprs) "a vector for its binding"
     (even? (count seq-exprs)) "an even number of forms in binding vector")
  (let [to-groups (core/fn [seq-exprs]
                    (reduce1 (core/fn [groups [k v]]
                              (if (keyword? k)
                                (conj (pop groups) (conj (peek groups) [k v]))
                                (conj groups [k v])))
                            [] (partition 2 seq-exprs)))
        err (core/fn [& msg] (throw (IllegalArgumentException. ^String (apply str msg))))
        emit-bind (core/fn emit-bind [[[bind expr & mod-pairs]
                                  & [[_ next-expr] :as next-groups]]]
                    (let [giter (gensym "iter__")
                          gxs (gensym "s__")
                          do-mod (core/fn do-mod [[[k v :as pair] & etc]]
                                   (cond
                                     (= k :let) `(let ~v ~(do-mod etc))
                                     (= k :while) `(when ~v ~(do-mod etc))
                                     (= k :when) `(if ~v
                                                    ~(do-mod etc)
                                                    (recur (rest ~gxs)))
                                     (keyword? k) (err "Invalid 'for' keyword " k)
                                     next-groups
                                      `(let [iterys# ~(emit-bind next-groups)
                                             fs# (seq (iterys# ~next-expr))]
                                         (if fs#
                                           (concat fs# (~giter (rest ~gxs)))
                                           (recur (rest ~gxs))))
                                     :else `(cons ~body-expr
                                                  (~giter (rest ~gxs)))))]
                      (if next-groups
                        #_"not the inner-most loop"
                        `(fn ~giter [~gxs]
                           (lazy-seq
                             (loop [~gxs ~gxs]
                               (when-first [~bind ~gxs]
                                 ~(do-mod mod-pairs)))))
                        #_"inner-most loop"
                        (let [gi (gensym "i__")
                              gb (gensym "b__")
                              do-cmod (core/fn do-cmod [[[k v :as pair] & etc]]
                                        (cond
                                          (= k :let) `(let ~v ~(do-cmod etc))
                                          (= k :while) `(when ~v ~(do-cmod etc))
                                          (= k :when) `(if ~v
                                                         ~(do-cmod etc)
                                                         (recur
                                                           (unchecked-inc ~gi)))
                                          (keyword? k)
                                            (err "Invalid 'for' keyword " k)
                                          :else
                                            `(do (chunk-append ~gb ~body-expr)
                                                 (recur (unchecked-inc ~gi)))))]
                          `(fn ~giter [~gxs]
                             (lazy-seq
                               (loop [~gxs ~gxs]
                                 (when-let [~gxs (seq ~gxs)]
                                   (if (chunked-seq? ~gxs)
                                     (let [c# (chunk-first ~gxs)
                                           size# (int (count c#))
                                           ~gb (chunk-buffer size#)]
                                       (if (loop [~gi (int 0)]
                                             (if (< ~gi size#)
                                               (let [~bind (.nth c# ~gi)]
                                                 ~(do-cmod mod-pairs))
                                               true))
                                         (chunk-cons
                                           (chunk ~gb)
                                           (~giter (chunk-rest ~gxs)))
                                         (chunk-cons (chunk ~gb) nil)))
                                     (let [~bind (first ~gxs)]
                                       ~(do-mod mod-pairs)))))))))))]
    `(let [iter# ~(emit-bind (to-groups seq-exprs))]
       (iter# ~(second seq-exprs)))))

(defmacro if-let
  "bindings => binding-form test

  If test is true, evaluates then with binding-form bound to the value of 
  test, if not, yields else"
  {:added "1.0"}
  ([bindings then]
   `(if-let ~bindings ~then nil))
  ([bindings then else & oldform]
   (assert-args
     (vector? bindings) "a vector for its binding"
     (nil? oldform) "1 or 2 forms after binding vector"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
   (let [form (bindings 0) tst (bindings 1)]
     `(let [temp# ~tst]
        (if temp#
          (let [~form temp#]
            ~then)
          ~else)))))

(defmacro if-not
  "Evaluates test. If logical false, evaluates and returns then expr, 
  otherwise else expr, if supplied, else nil."
  {:added "1.0"}
  ([test then] `(if-not ~test ~then nil))
  ([test then else]
     `(if (not ~test) ~then ~else)))

(defmacro letfn 
  "fnspec ==> (fname [params*] exprs) or (fname ([params*] exprs)+)

  Takes a vector of function specs and a body, and generates a set of
  bindings of functions to their names. All of the names are available
  in all of the definitions of the functions, as well as the body."
  {:added "1.0", :forms '[(letfn [fnspecs*] exprs*)],
   :special-form true, :url nil}
  [fnspecs & body] 
  `(letfn* ~(vec (interleave (map first fnspecs) 
                             (map #(cons `fn %) fnspecs)))
           ~@body))

(defmacro memfn
  "Expands into code that creates a fn that expects to be passed an
  object and any args and calls the named instance method on the
  object passing the args. Use when you want to treat a Java method as
  a first-class fn. name may be type-hinted with the method receiver's
  type in order to avoid reflective calls."
  {:added "1.0"}
  [name & args]
  (let [t (with-meta (gensym "target")
            (meta name))]
    `(fn [~t ~@args]
       (. ~t (~name ~@args)))))

(defmacro or
  "Evaluates exprs one at a time, from left to right. If a form
  returns a logical true value, or returns that value and doesn't
  evaluate any of the other expressions, otherwise it returns the
  value of the last expression. (or) returns nil."
  {:added "1.0"}
  ([] nil)
  ([x] x)
  ([x & next]
      `(let [or# ~x]
         (if or# or# (or ~@next)))))

(defmacro when
  "Evaluates test. If logical true, evaluates body in an implicit do."
  {:added "1.0"}
  [test & body]
  (list 'if test (cons 'do body)))

(defmacro when-first
  "bindings => x xs

  Roughly the same as (when (seq xs) (let [x (first xs)] body)) but xs is evaluated only once"
  {:added "1.0"}
  [bindings & body]
  (assert-args
     (vector? bindings) "a vector for its binding"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
  (let [[x xs] bindings]
    `(when-let [xs# (seq ~xs)]
       (let [~x (first xs#)]
         ~@body))))

(defmacro when-let
  "bindings => binding-form test

  When test is true, evaluates body with binding-form bound to the value of test"
  {:added "1.0"}
  [bindings & body]
  (assert-args
     (vector? bindings) "a vector for its binding"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
   (let [form (bindings 0) tst (bindings 1)]
    `(let [temp# ~tst]
       (when temp#
         (let [~form temp#]
           ~@body)))))

(defmacro when-not
  "Evaluates test. If logical false, evaluates body in an implicit do."
  {:added "1.0"}
  [test & body]
  (list 'if test nil (cons 'do body)))

(defmacro while
  "Repeatedly executes body while test expression is true. Presumes
  some side-effect will cause test to become false/nil. Returns nil"
  {:added "1.0"}
  [test & body]
  `(loop []
     (when ~test
       ~@body
       (recur))))

(def protocol-hints (atom {}))
(defmacro add-protocol-hints!
  "given a map of fast-path hints for macro-expansion of protocol functions. Expands to nothing."
  [hints]
  (swap! protocol-hints (constantly {})) ;TODO don't obliterate hints on every new file?
  (swap! protocol-hints (core/fn [ph] (merge-with (core/fn [o n] (conj (or o []) n)) ph hints)))
  nil
  #_(core/let [forward-types (reduce (core/fn [a [k v]] (into a v)) #{} hints)] ;generate forward defs for typechecks
      `(do ~@(map (core/fn [ft] `(def ~(symbol (str (name ft) "?")))) forward-types))))

(def compare-arglist
  (comparator
   (fn* [arglist-a arglist-b]
     (if (some #{'&} arglist-a)
       false
       (if (some #{'&} arglist-b)
         true
         (core/< (count arglist-a) (count arglist-b)))))))

(defmacro js* [& forms])
(defmacro scm-str* [& forms])
(defmacro scm* [& forms] (case condc/*target-platform* :gambit (cons 'scm* forms)))

(defmacro scm-boolean*
  "easy way to get unchecked scheme tests. Metadata is stored on the scm* symbol."
  [& forms]
  `(~(with-meta 'scm* {:tag 'boolean}) ~@forms))

(core/defn
  maybe-destructured
  [params body]
  (if (every? symbol? params)
    (cons params body)
    (core/loop [params params
           new-params []
           lets []]
      (if params
        (if (symbol? (first params))
          (recur (next params) (conj new-params (first params)) lets)
          (core/let [gparam (gensym "p__")]
            (recur (next params) (conj new-params gparam)
                   (-> lets (conj (first params)) (conj gparam)))))
        `(~new-params
          (let ~lets
            ~@body))))))

(core/defn destructure [bindings]
  (core/let [bents (partition 2 bindings)
         pb (core/fn pb [bvec b v]
              (core/let [pvec
                     (core/fn [bvec b val]
                       (core/let [gvec (gensym "vec__")]
                         (core/loop [ret (-> bvec (conj gvec) (conj val))
                                     n 0
                                     bs b
                                     seen-rest? false]
                           (if (core/seq bs)
                             (core/let [firstb (first bs)]
                               (cond
                                 (= firstb '&) (recur (pb ret (second bs) (list `nthnext gvec n))
                                                      n
                                                      (nnext bs)
                                                      true)
                                 (= firstb :as) (pb ret (second bs) gvec)
                                 :else (if seen-rest?
                                         (throw (new Exception "Unsupported binding form, only :as can follow & parameter"))
                                         (recur (pb ret firstb  (list `nth gvec n nil))
                                                (core/inc n)
                                                (next bs)
                                                seen-rest?))))
                             ret))))
                     pmap
                     (core/fn [bvec b v]
                       (core/let [gmap (gensym "map__")
                                  defaults (:or b)]
                         (core/loop [ret (-> bvec (conj gmap) (conj v)
                                             (conj gmap) (conj `(if (core/seq? ~gmap) (apply hash-map ~gmap) ~gmap))
                                             ((core/fn [ret]
                                                (if (:as b)
                                                  (conj ret (:as b) gmap)
                                                  ret))))
                                     bes (reduce
                                          (core/fn [bes entry]
                                            (reduce #(assoc %1 %2 ((val entry) %2))
                                                    (dissoc bes (key entry))
                                                    ((key entry) bes)))
                                          (dissoc b :as :or)
                                          {:keys #(keyword (core/str %)), :strs core/str, :syms #(list `quote %)})]
                           (if (core/seq bes)
                             (core/let [bb (key (first bes))
                                        bk (val (first bes))
                                        has-default (contains? defaults bb)]
                               (recur (pb ret bb (if has-default
                                                   (list `get gmap bk (defaults bb))
                                                   (list `get gmap bk)))
                                      (next bes)))
                             ret))))]
                    (cond
                      (symbol? b) (-> bvec (conj b) (conj v))
                      (vector? b) (pvec bvec b v)
                      (map? b) (pmap bvec b v)
                      :else (throw (new Exception (core/str "Unsupported binding form: " b))))))
         process-entry (core/fn [bvec b] (pb bvec (first b) (second b)))]
        (if (every? symbol? (map first bents))
          bindings
          (reduce process-entry [] bents))))

(defmacro let
  "binding => binding-form init-expr

  Evaluates the exprs in a lexical context in which the symbols in
  the binding-forms are bound to their respective init-exprs or parts
  therein."
  [bindings & body]
  (assert-args
     (vector? bindings) "a vector for its binding"
     (even? (count bindings)) "an even number of forms in binding vector")
  `(let* ~(destructure bindings) ~@body))

(defmacro loop
  "Evaluates the exprs in a lexical context in which the symbols in
  the binding-forms are bound to their respective init-exprs or parts
  therein. Acts as a recur target."
  [bindings & body]
    (assert-args
      (vector? bindings) "a vector for its binding"
      (even? (count bindings)) "an even number of forms in binding vector")
    (core/let [db (destructure bindings)]
      (if (= db bindings)
        `(loop* ~bindings ~@body)
        (core/let [vs (take-nth 2 (drop 1 bindings))
              bs (take-nth 2 bindings)
              gs (map (core/fn [b] (if (symbol? b) b (gensym))) bs)
              bfs (reduce (core/fn [ret [b v g]]
                            (if (symbol? b)
                              (conj ret g v)
                              (conj ret g v b g)))
                          [] (map vector bs vs gs))]
          `(let ~bfs
             (loop* ~(vec (interleave gs gs))
               (let ~(vec (interleave bs gs))
                 ~@body)))))))

(def fast-path-protocols
  "protocol fqn -> [partition number, bit]"
  (zipmap (map #(symbol "cljscm.core" (core/str %))
               '[IFn ICounted IEmptyableCollection ICollection IIndexed ASeq ISeq INext
                 ILookup IAssociative IMap IMapEntry ISet IStack IVector IDeref
                 IDerefWithTimeout IMeta IWithMeta IReduce IKVReduce IEquiv IHash
                 ISeqable ISequential IList IRecord IReversible ISorted IPrintable IWriter
                 IPrintWithWriter IPending IWatchable IEditableCollection ITransientCollection
                 ITransientAssociative ITransientMap ITransientVector ITransientSet
                 IMultiFn IChunkedSeq IChunkedNext IComparable])
          (iterate (core/fn [[p b]]
                     (if (core/== 2147483648 b)
                       [(core/inc p) 1]
                       [p (core/bit-shift-left b 1)]))
                   [0 1])))

(def fast-path-protocol-partitions-count
  "total number of partitions"
  (core/let [c (count fast-path-protocols)
        m (core/mod c 32)]
    (if (core/zero? m)
      (core/quot c 32)
      (core/inc (core/quot c 32)))))

(core/defn bool-expr [e]
  (vary-meta e assoc :tag 'boolean))

;; internal - do not use.
(defmacro coercive-not [x]
   (bool-expr `(scm-boolean* {:x ~x}
                     ~'(not :x))))

;; internal - do not use.
(defmacro coercive-not= [x y]
  (bool-expr `(scm* {:x ~x :y ~y}
                    ~'(not (eqv? :x :y)))))

;; internal - do not use.
(defmacro coercive-= [x y]
  (bool-expr `(scm* {:x ~x :y ~y}
                    ~'(equal? :x :y))))

(core/defn scm-number? [x] `(scm-boolean* {:x ~x} ~'(number? :x)))
(core/defn scm-pair? [x] `(scm-boolean* {:x ~x} ~'(pair? :x)))
(core/defn scm-boolean? [x] `(scm-boolean* {:x ~x} ~'(boolean? :x)))
(core/defn scm-nil? [x] (with-meta `(identical? nil ~x) {:tag :boolean}))
(core/defn scm-null? [x] `(identical? () ~x))
(core/defn scm-char? [x] `(scm-boolean* {:x ~x} ~'(char? :x)))
(core/defn scm-vector? [x] `(scm-boolean* {:x ~x} ~'(vector? :x)))
(core/defn scm-symbol? [x] `(scm-boolean* {:x ~x} ~'(symbol? :x)))
(core/defn scm-keyword? [x] `(scm-boolean* {:x ~x} ~'(keyword? :x)))
(core/defn scm-procedure? [x] `(scm-boolean* {:x ~x} ~'(procedure? :x)))
(core/defn scm-string? [x] `(scm-boolean* {:x ~x} ~'(string? :x)))
(core/defn scm-table? [x] `(scm-boolean* {:x ~x} ~'(table? :x)))

(core/defn scm-instance?*
  "aware of builtin scheme primitive type tests. lookup with namespace-qualified
  type symbols only."
  [type-sym o]
  ((clojure.core/get {'cljscm.core/Number scm-number?
                      'cljscm.core/Pair scm-pair?
                      'cljscm.core/Boolean scm-boolean?
                      'cljscm.core/Nil scm-nil?
                      'cljscm.core/Null scm-null?
                      'cljscm.core/Char scm-char?
                      'cljscm.core/Array scm-vector?
                      'cljscm.core/Symbol scm-symbol?
                      'cljscm.core/Keyword scm-keyword?
                      'cljscm.core/Procedure scm-procedure?
                      'cljscm.core/String scm-string?
                      'cljscm.core/Table scm-table?}
                     type-sym
                     (core/fn [o] `(scm-boolean* {:o ~o} (~(symbol (str type-sym "?")) :o))))
   o))

(defmacro scm-instance?
  "giveen the resolved symbol of the type, generates a fast type check."
  [type-sym o]
  (scm-instance?* type-sym o))

(defmacro scm-length [lst]
  `(scm* {:lst ~lst} ~'(length :lst)))

(defmacro scm-nillify [lst]
  `(scm* {:lst ~lst :nil ~nil} ~'(if (null? :lst) :nil :lst)))

(core/defn make-car [lst cnt]
  (if (core/<= cnt 3) (core/list (core/symbol (core/str "ca" (apply str (repeat cnt "d")) "r")) lst)
      (core/list 'list-ref lst cnt)))

(core/defn make-cdr [lst cnt]
  (if (core/= cnt 0) lst
      (if (core/<= cnt 4) (core/list (core/symbol (core/str "c" (apply str (repeat cnt "d")) "r")) lst)
          (make-cdr (make-cdr lst 4) (core/- cnt 4)))))

(core/defn- combine-arities
  [methods]
  (let [[smallest-meth & other-meths :as meths] (sort-by first compare-arglist methods)
        middle-meths (butlast other-meths)
        biggest-meth (last other-meths)
        num-shared (count (first smallest-meth))
        restparam (gensym "rest")
        find-inits
        , (core/fn [meth]
            (let [args (first meth)
                  variadic? (some #{'&} args)
                  shared-args (take num-shared args)
                  fixed-args (if variadic?
                               (->> args (drop num-shared) (drop-last 2))
                               (drop num-shared args))
                  variadic-arg (when variadic? (last args))
                  inits (concat
                         (map vector shared-args (first smallest-meth))
                         (map-indexed (core/fn [i a] [a `(~'scm* [~restparam] ~(make-car restparam i))])
                                      fixed-args)
                         (when variadic? [[variadic-arg
                                             `(scm-nillify (~'scm* [~restparam] ~(make-cdr restparam (core/count fixed-args))))]]))]
              {:args (map first inits)
               :inits (map second inits)
               :implforms (rest meth) 
               :restcount (when (not variadic-arg) (core/count fixed-args))}))]
    {:meths (map find-inits meths)
     :restparam restparam
     :num-shared num-shared 
     :combined-argform (vec (concat (first smallest-meth) ['& restparam]))}))

;-- compiles variadic methods into a case dispatch.
(defmacro fn
  "params => positional-params* , or positional-params* & next-param
  positional-param => binding-form
  next-param => binding-form
  name => symbol

  Defines a function"
  {:added "1.0", :special-form true,
   :forms '[(fn name? [params* ] exprs*) (fn name? ([params* ] exprs*)+)]}
  [& sigs]
  (core/let [name (if (symbol? (first sigs)) (first sigs) nil)
             sigs (if name (next sigs) sigs)
             sigs (if (vector? (first sigs)) (list sigs) sigs)
             psig (fn* [sig]
                       (core/let [[params & body] sig
                                  conds (when (and (next body) (map? (first body))) 
                                          (first body))
                                  body (if conds (next body) body)
                                  conds (or conds (meta params))
                                  pre (:pre conds)
                                  post (:post conds)                       
                                  body (if post
                                         `((let [~'% ~(if (core/< 1 (count body)) 
                                                        `(do ~@body) 
                                                        (first body))]
                                             ~@(map (fn* [c] `(assert ~c)) post)
                                             ~'%))
                                         body)
                                  body (if pre
                                         (concat (map (fn* [c] `(assert ~c)) pre) 
                                                 body)
                                         body)]
                         (maybe-destructured params body)))
             dsigs (map psig sigs)
             any-variadic? (core/some #(some #{'&} (first %)) dsigs)
             single-arity? (core/<= (count dsigs) 1)
             combined-form
             , (if single-arity?
                 dsigs
                 (let [{:keys [meths combined-argform restparam num-shared]}
                       , (combine-arities dsigs)
                       meths (if any-variadic?
                               meths
                               (concat meths [{:implforms `(throw (cljscm.core/Error. (str "No arity method for " (+ ~num-shared (scm-length ~restparam)) " args.")))}]))]
                   `(~combined-argform
                     (~'case (scm-length ~restparam)
                       ~@(mapcat (core/fn [{:keys [args inits implforms restcount]}]
                                   (let [initpairs
                                         , (->> (map vector args inits)
                                                ;(filter (core/fn [[a b]] (not= a b))) ; no re-binding needed if method arg is same as the combined arg. (but we need a potential recur target, so we need to bind everything for now.)
                                                (apply concat)
                                                (core/seq)) 
                                         body (if true ;initpairs
                                                `(~'loop [~@initpairs]
                                                   ~@implforms)
                                                `(do ~@implforms))]
                                     (if restcount
                                       [restcount body]
                                       [body])))
                                 meths)))))]
    (with-meta
      (if name
        (list* 'fn* name combined-form)
        (cons 'fn* combined-form))
      (assoc (meta &form)
        :single-arity single-arity?))))

;; internal do not use
(defmacro unsafe-bit-and
  ([x y] (bool-expr (list 'js* "(~{} & ~{})" x y)))
  ([x y & more] `(unsafe-bit-and (unsafe-bit-and ~x ~y) ~@more)))

#_(defmacro bit-clear [x n]
  (list 'js* "(~{} & ~(1 << ~{}))" x n))

#_(defmacro bit-flip [x n]
  (list 'js* "(~{} ^ (1 << ~{}))" x n))

#_(defmacro bit-test [x n]
  (list 'js* "((~{} & (1 << ~{})) != 0)" x n))


#_(defmacro bit-set [x n]
    (list 'js* "(~{} | (1 << ~{}))" x n))

#_(defmacro get
  ([coll k]
     `(-lookup ~coll ~k nil))
  ([coll k not-found]
     `(-lookup ~coll ~k ~not-found)))

(defmacro caching-hash [coll hash-fn hash-key]
  `(let [h# ~hash-key]
     (if-not (nil? h#)
       h#
       (let [h# (~hash-fn ~coll)]
         (set! ~hash-key h#)
         h#))))

;;; internal -- reducers-related macros

(core/defn- do-curried
  [name doc meta args body]
  (core/let [cargs (vec (butlast args))]
    `(defn ~name ~doc ~meta
       (~cargs (core/fn [x#] (~name ~@cargs x#)))
       (~args ~@body))))

(defmacro ^:private defcurried
  "Builds another arity of the fn that returns a fn awaiting the last
  param"
  [name doc meta args & body]
  (do-curried name doc meta args body))

(core/defn- do-rfn [f1 k fkv]
  `(core/fn
     ([] (~f1))
     ~(clojure.walk/postwalk
       #(if (sequential? %)
          ((if (vector? %) vec identity)
           (core/remove #{k} %))
          %)
       fkv)
     ~fkv))

(defmacro ^:private rfn
  "Builds 3-arity reducing fn given names of wrapped fn and key, and k/v impl."
  [[f1 k] fkv]
  (do-rfn f1 k fkv))

;;; end of reducers macros

(core/defn protocol-prefix [psym]
  (core/str (apply str (->> (core/str psym)
                            (map #(get {\. \$} % %))
                            (map #(get {\/ \$} % %))))
            "$"))

#_(def #^:private base-type
     {nil "null"
      'object "object"
      'string "string"
      'number "number"
      'array "array"
      'function "function"
      'boolean "boolean"
      'default "_"})

(defmacro reify [& impls]
  (core/let [t      (gensym "t")
        meta-sym (gensym "meta")
        this-sym (gensym "_")
        locals (keys (:locals &env))
        ns     (-> &env :ns :name)
        munge  str ;cljscm.compiler/munge
        ns-t   (list 'js* (core/str (munge ns) "." (munge t)))]
    `(do
       (when (undefined? ~ns-t)
         (deftype ~t [~@locals ~meta-sym]
           IWithMeta
           (~'-with-meta [~this-sym ~meta-sym]
             (new ~t ~@locals ~meta-sym))
           IMeta
           (~'-meta [~this-sym] ~meta-sym)
           ~@impls))
       (new ~t ~@locals nil))))

#_(defmacro this-as
  "Defines a scope where JavaScript's implicit \"this\" is bound to the name provided."
  [name & body]
  `(let [~name (~'js* "this")]
     ~@body))

#_(core/defn to-property [sym]
  (symbol (core/str "-" sym)))

(core/defn gather-polysigs
  "[[x y][x y z]] -> [x y & rst]"
  [sigs]
  (core/let [smallest-sig (first (sort-by count sigs))]
    (vec (concat smallest-sig
            (if (clojure.core/> (count sigs) 1) ['& 'rst] [])))))

(core/defn- parse-impls [specs]
  (core/loop [ret {} s specs]
    (if (core/seq s)
      (recur (assoc ret (first s) (take-while seq? (next s)))
             (drop-while seq? (next s)))
      ret)))

;It seems this clobbers multi-variadic protocol methods?
(core/defn- emit-hinted-impl [c [p fs]]
  (core/let [hint (core/fn [specs]
               (core/let [specs (if (vector? (first specs)) 
                             (list specs) 
                             specs)]
                 (map (core/fn [[[target & args] & body]]
                               (cons (apply vector (vary-meta target assoc :tag c) args)
                                     body))
                      specs)))]
    [p
     (apply merge-with (core/fn [old new] (concat old (drop 2 new)))
            (map (core/fn [[f & r :as fr]] {(-> f name keyword)
                              (with-meta (cons 'fn (cons f (hint r))) (assoc (meta fr) :protocol-impl true))})
                 fs))
     #_(zipmap (map #(-> % first name keyword) fs)
               (map #(cons 'fn (hint (drop 1 %))) fs))]))

(core/defn- emit-extend-type [c specs]
  (core/let [impls (parse-impls specs)]
    (apply list 'extend c
                        (mapcat (partial emit-hinted-impl c) impls))))

(defmacro extend-type 
  "A macro that expands into an extend call. Useful when you are
  supplying the definitions explicitly inline, extend-type
  automatically creates the maps required by extend.  Propagates the
  class as a type hint on the first argument of all fns.

  (extend-type MyType 
    Countable
      (cnt [c] ...)
    Foo
      (bar [x y] ...)
      (baz ([x] ...) ([x y & zs] ...)))

  expands into:

  (extend MyType
   Countable
     {:cnt (fn [c] ...)}
   Foo
     {:baz (fn ([x] ...) ([x y & zs] ...))
      :bar (fn [x y] ...)})"
  {:added "1.2"} 
  [t & specs]
  (emit-extend-type t specs))

#_(defmacro extend-type [tsym & impls]
  (core/let [resolve #(core/let [ret (:name (ana/resolve-var (dissoc &env :locals) %))]
                   (assert ret (core/str "Can't resolve: " %))
                   ret)
        impl-map (core/loop [ret {} s impls]
                   (if (core/seq s)
                     (recur (assoc ret (first s) (take-while seq? (next s)))
                            (drop-while seq? (next s)))
                     ret))
        warn-if-not-protocol #(when-not (= 'Object %)
                                (if ana/*cljs-warn-on-undeclared*
                                  (if-let [var (ana/resolve-existing-var (dissoc &env :locals) %)]
                                    (do
                                     (when-not (:protocol-symbol var)
                                       (ana/warning &env
                                         (core/str "WARNING: Symbol " % " is not a protocol")))
                                     (when (and ana/*cljs-warn-protocol-deprecated*
                                                (-> var :deprecated)
                                                (not (-> % meta :deprecation-nowarn)))
                                       (ana/warning &env
                                         (core/str "WARNING: Protocol " % " is deprecated"))))
                                    (ana/warning &env
                                      (core/str "WARNING: Can't resolve protocol symbol " %)))))
        skip-flag (set (-> tsym meta :skip-protocol-flag))]
    (if (base-type tsym)
      (core/let [t (base-type tsym)
                        assign-impls (core/fn [[p sigs]]
                           (warn-if-not-protocol p)
                           (core/let [psym (resolve p)
                                 pfn-prefix (subs (core/str psym) 0 (clojure.core/inc (.indexOf (core/str psym) "/")))]
                             (cons `(aset ~psym ~t true)
                                   (map (core/fn [[f & meths :as form]]
                                          `(aset ~(symbol (core/str pfn-prefix f)) ~t ~(with-meta `(fn ~@meths) (meta form))))
                                        sigs))))]
        `(do ~@(mapcat assign-impls impl-map)))
      (core/let [t (resolve tsym)
            prototype-prefix (core/fn [sym]
                               `(.. ~tsym -prototype ~(to-property sym)))
            assign-impls (core/fn [[p sigs]]
                           (warn-if-not-protocol p)
                           (core/let [psym (resolve p)
                                 pprefix (protocol-prefix psym)]
                             (if (= p 'Object)
                               (core/let [adapt-params (core/fn [[sig & body]]
                                                    (core/let [[tname & args] sig]
                                                      (list (vec args) (list* 'this-as (vary-meta tname assoc :tag t) body))))]
                                 (map (core/fn [[f & meths :as form]]
                                        `(set! ~(prototype-prefix f)
                                               ~(with-meta `(fn ~@(map adapt-params meths)) (meta form))))
                                      sigs))
                               (concat (when-not (skip-flag psym)
                                         [`(set! ~(prototype-prefix pprefix) true)])
                                       (mapcat (core/fn [[f & meths :as form]]
                                                 (if (= psym 'cljscm.core/IFn)
                                                   (core/let [adapt-params (core/fn [[[targ & args :as sig] & body]]
                                                                        (core/let [this-sym (with-meta 'self__ {:tag t})]
                                                                          `(~(vec (cons this-sym args))
                                                                            (this-as ~this-sym
                                                                                     (let [~targ ~this-sym]
                                                                                       ~@body)))))
                                                         meths (map adapt-params meths)
                                                         this-sym (with-meta 'self__ {:tag t})
                                                         argsym (gensym "args")]
                                                     [`(set! ~(prototype-prefix 'call) ~(with-meta `(fn ~@meths) (meta form)))
                                                      `(set! ~(prototype-prefix 'apply)
                                                             ~(with-meta
                                                                `(fn ~[this-sym argsym]
                                                                   (.apply (.-call ~this-sym) ~this-sym
                                                                           (.concat (array ~this-sym) (aclone ~argsym))))
                                                                (meta form)))])
                                                   (core/let [pf (core/str pprefix f)
                                                         adapt-params (core/fn [[[targ & args :as sig] & body]]
                                                                        (cons (vec (cons (vary-meta targ assoc :tag t) args))
                                                                              body))]
                                                     (if (vector? (first meths))
                                                       [`(set! ~(prototype-prefix (core/str pf "$arity$" (count (first meths)))) ~(with-meta `(fn ~@(adapt-params meths)) (meta form)))]
                                                       (map (core/fn [[sig & body :as meth]]
                                                              `(set! ~(prototype-prefix (core/str pf "$arity$" (count sig)))
                                                                     ~(with-meta `(fn ~(adapt-params meth)) (meta form))))
                                                            meths)))))
                                               sigs)))))]
        `(do ~@(mapcat assign-impls impl-map))))))

(core/defn- prepare-protocol-masks [env t impls]
  (core/let [resolve #(core/let [ret (:name (ana/resolve-var (dissoc env :locals) %))]
                   (assert ret (core/str "Can't resolve: " %))
                   ret)
        impl-map (core/loop [ret {} s impls]
                   (if (core/seq s)
                     (recur (assoc ret (first s) (take-while seq? (next s)))
                            (drop-while seq? (next s)))
                     ret))]
    (if-let [fpp-pbs (core/seq (keep fast-path-protocols
                                (map resolve
                                     (keys impl-map))))]
      (core/let [fpps (into #{} (filter (partial contains? fast-path-protocols)
                                   (map resolve
                                        (keys impl-map))))
            fpp-partitions (group-by first fpp-pbs)
            fpp-partitions (into {} (map (juxt key (comp (partial map peek) val))
                                         fpp-partitions))
            fpp-partitions (into {} (map (juxt key (comp (partial reduce core/bit-or) val))
                                         fpp-partitions))]
        [fpps
         (reduce (core/fn [ps p]
                   (update-in ps [p] (fnil identity 0)))
                 fpp-partitions
                 (range fast-path-protocol-partitions-count))]))))

(core/defn dt->et
  ([t specs fields] (dt->et t specs fields false))
  ([t specs fields inline]
     (core/letfn [(inline-arity? [sig] (not (vector? (second sig))))]
       (core/loop [ret [] s specs]
         (if (core/seq s)
           (recur (-> ret
                      (conj (first s))
                      (into
                       (reduce (core/fn [v [f sigs]]
                                 (conj v (vary-meta (cons f (mapcat #(if (inline-arity? %)
                                                                       (drop 1 %)
                                                                       (list (drop 1 %))) sigs))
                                                    assoc :cljscm.analyzer/type t
                                                    :cljscm.analyzer/fields fields
                                                    :protocol-impl true
                                                    :protocol-inline inline)))
                               []
                               (group-by first (take-while seq? (next s))))))
                  (drop-while seq? (next s)))
           ret)))))

(core/defn collect-protocols [impls env]
  (->> impls
      (filter symbol?)
      (map #(:name (ana/resolve-var (dissoc env :locals) %)))
      (into #{})))

(defmacro deftype [t fields & impls]
  (core/let [r (:name (ana/resolve-var (dissoc &env :locals) t))
        [fpps pmasks] (prepare-protocol-masks &env t impls)
        protocols (collect-protocols impls &env)
        t (vary-meta t assoc
            :protocols protocols
            :skip-protocol-flag fpps) ]
    (if (core/seq impls)
      `(do
         (deftype* ~t ~fields ~pmasks)
;         (set! (.-cljs$lang$type ~t) true)
;         (set! (.-cljs$lang$ctorPrSeq ~t) (core/fn [this#] (list ~(core/str r))))
;         (set! (.-cljs$lang$ctorPrWriter ~t) (core/fn [this# writer# opt#] (-write writer# ~(core/str r))))
         (extend-type ~t ~@(dt->et t impls fields true))
         ~t)
      `(do
         (deftype* ~t ~fields ~pmasks)
;         (set! (.-cljs$lang$type ~t) true)
;         (set! (.-cljs$lang$ctorPrSeq ~t) (core/fn [this#] (list ~(core/str r))))
;         (set! (.-cljs$lang$ctorPrWriter ~t) (core/fn [this# writer# opts#] (-write writer# ~(core/str r))))
         ~t))))

(core/defn- emit-defrecord
  "Do not use this directly - use defrecord"
  [env tagname rname fields impls]
  (core/let [hinted-fields fields
        fields (vec (map #(with-meta % nil) fields))
        base-fields fields
        #_"depends on Gambit's record lookup i.e. cljscm.core/MyRecord-__meta"
        accessorize (core/fn [this fld] `(~(symbol (str tagname "-" fld)) ~this))
        fields (conj fields '__meta '__extmap)] ; (with-meta '__hash {:mutable true})
    (core/let [gs (gensym)
          ksym (gensym "k")
          gthis (gensym 'this)
          impls (concat
                 impls
                 ['IRecord
                  'IHash
                  `(~'-hash [this#] (hash-coll this#))
                  'IEquiv
                  `(~'-equiv [this# other#]
                             (and (identical? (type this#)
                                              (type other#))
                                  (equiv-map this# other#)))
                  'IMeta
                  `(~'-meta [~gthis] ~(accessorize gthis '__meta))
                  'IWithMeta
                  `(~'-with-meta [~gthis ~gs] (new ~tagname ~@(map #(accessorize gthis %) base-fields) ~gs ~(accessorize gthis '__extmap)))
                  'ILookup
                  `(~'-lookup [this# k#] (-lookup this# k# nil))
                  `(~'-lookup [~gthis ~ksym else#]
                              (get (merge (hash-map ~@(mapcat
                                                       (core/fn [fld] [(keyword fld) (accessorize gthis fld)]) 
                                                       base-fields))
                                          ~(accessorize gthis '__extmap))
                                   ~ksym else#))
                  `(~'-lookup [~gthis ~ksym else#]
                              (cond
                               ~@(mapcat (core/fn [f] [`(identical? ~ksym ~(keyword f)) (accessorize gthis f)]) base-fields)
                               :else (get ~(accessorize gthis '__extmap) ~ksym else#)))
                  'ICounted
                  `(~'-count [~gthis] (+ ~(count base-fields) (count ~(accessorize gthis '__extmap))))
                  'ICollection
                  `(~'-conj [this# entry#]
                            (if (vector? entry#)
                              (-assoc this# (-nth entry# 0) (-nth entry# 1))
                              (reduce -conj
                                      this#
                                      entry#)))
                  'IAssociative
                  `(~'-assoc [~gthis k# ~gs]
                             (condp identical? k#
                               ~@(mapcat (core/fn [fld]
                                                  [(keyword fld) (list* `new tagname (map #(core/get {fld gs} % (accessorize gthis %)) fields))])
                                         base-fields)
                               (new ~tagname ~@(map #(accessorize gthis %) (remove #{'__extmap} fields)) (assoc ~(accessorize gthis '__extmap) k# ~gs))))
                  'IMap
                  `(~'-dissoc [~gthis k#] (if (contains? #{~@(map keyword base-fields)} k#)
                                            (dissoc (with-meta (into {} ~gthis) ~(accessorize gthis '__meta)) k#)
                                            (new ~tagname ~@(map #(accessorize gthis %) (remove #{'__extmap} fields)) 
                                                 (not-empty (dissoc ~(accessorize gthis '__extmap) k#)))))
                  'ISeqable
                  `(~'-seq [~gthis] (core/seq (concat [~@(map #(list `vector (keyword %) (accessorize gthis %)) base-fields)] 
                                                 ~(accessorize gthis '__extmap))))
                  
                  'IPrintWithWriter
                  `(~'-pr-writer [gthis writer# opts#]
                                 (let [pr-pair# (core/fn [keyval#] (pr-sequential-writer writer# pr-writer "" " " "" opts# keyval#))]
                                   (pr-sequential-writer
                                    writer# pr-pair# (core/str "#" ~(name rname) "{") ", " "}" opts#
                                    (concat [~@(map #(list `vector (keyword %) (accessorize gthis %)) base-fields)]
                                            ~(accessorize gthis '__extmap)))))
                  ])
          [fpps pmasks] (prepare-protocol-masks env tagname impls)
          protocols (collect-protocols impls env)
          tagname (vary-meta tagname assoc
                             :protocols protocols
                             :skip-protocol-flag fpps)
          extended-constructor-name (symbol (str "make-" ana/*cljs-ns* "/" tagname "-extended"))
          poly-constructor-name (symbol (str "make-" ana/*cljs-ns* "/" tagname))
          poly-constructor `(fn
                              ([~@base-fields] (~extended-constructor-name ~@base-fields nil nil))
                              ([~@fields] (~extended-constructor-name ~@fields)))]
      `(do
         (~'scm* {::tagname ~tagname
                  ::constructor :constructor
                  ::init :init
                  ::nil nil}
                 (~'define-type ::tagname ::constructor ~extended-constructor-name
                   ~@fields))
         (~'scm* {::poly-constructor ~poly-constructor} (~'define ~poly-constructor-name ::poly-constructor))
         (def ~tagname (type (new ~tagname ~@(map (constantly nil) base-fields)))) ;hack in lieu of compiler hook to capture ##type-2-ns/tagname in scheme
         (~'scm* {::tagname ~tagname} (~'table-set! ~'cljscm.core/protocol-impls ::tagname ~'(make-table)))
         (extend-type ~tagname ~@(dt->et tagname impls fields true))))))

(core/defn- build-positional-factory
  [rsym rname fields]
  (core/let [fn-name (symbol (core/str '-> rsym))]
    `(defn ~fn-name
       [~@fields]
       (new ~rname ~@fields))))

(core/defn- build-map-factory
  [rsym rname fields]
  (core/let [fn-name (symbol (core/str 'map-> rsym))
	ms (gensym)
	ks (map keyword fields)
	getters (map (core/fn [k] `(~k ~ms)) ks)]
    `(defn ~fn-name
       [~ms]
       (new ~rname ~@getters nil (dissoc ~ms ~@ks)))))

(defmacro defrecord [rsym fields & impls]
  (core/let [r (:name (ana/resolve-var (dissoc &env :locals) rsym))]
    `(do
       ~(emit-defrecord &env rsym r fields impls)
       (set! (.-cljs$lang$ctorPrSeq ~r) (core/fn [this#] (list ~(core/str r))))
       (set! (.-cljs$lang$ctorPrWriter ~r) (core/fn [this# writer#] (-write writer# ~(core/str r))))
       ~(build-positional-factory rsym r fields)
       ~(build-map-factory rsym r fields)
       ~r)))

(core/defn gather-defns
  "([x] ..) ([x y] ..) -> ([x & rest] case-on-arg-count...)"
  [methods]
  (if (= 1 (count methods))
    `(defn ~methods)
    (let [sig (gather-polysigs (map first methods))]
      `(defn ~(gather-polysigs (map first methods))
         (case (clojure.core/count ~(last sig)
                                   ~(core/- (count sig) 2)) (rest (first methods)))))))

(defmacro defprotocol [psym & doc+methods]
  (core/let [p (:name (ana/resolve-var (dissoc &env :locals) psym))
             psym (vary-meta psym assoc :protocol-symbol true)
             ns-name (-> &env :ns :name)
             fqn (core/fn [n] (symbol (str ns-name "." n)))
             prefix (protocol-prefix p)
             methods (if (string? (first doc+methods)) (next doc+methods) doc+methods)
             expand-sig (core/fn [fname slot sig]
                          `(~sig
                            (if (and ~(first sig) (. ~(first sig) ~(symbol (core/str "-" slot)))) ;; Property access needed here.
                              (. ~(first sig) ~slot ~@sig)
                              (let [x# (if (nil? ~(first sig)) nil ~(first sig))]
                                ((or
                                  (aget ~(fqn fname) (goog.typeOf x#))
                                  (aget ~(fqn fname) "_")
                                  (throw (missing-protocol
                                          ~(core/str psym "." fname) ~(first sig))))
                                 ~@sig)))))
             #_method #_(core/fn [[fname & sigs]]
                          (core/let [sigs (take-while vector? sigs)
                                     slot (symbol (core/str prefix (name fname)))
                                     fname (vary-meta fname assoc :protocol p)]
                            `(defn ~fname ~@(map (core/fn [sig]
                                                   (expand-sig fname
                                                               (symbol (core/str slot "$arity$" (count sig)))
                                                               sig))
                                                 sigs))))
             known-implementing-types (clojure.core/get @protocol-hints p)
             known-implementing-type-set (into #{} known-implementing-types) ;(:name (ana/resolve-var (dissoc &env :locals) fname))
             check-impl (core/fn [sym] (not (clojure.core/get known-implementing-type-set sym)))
             mk-prim
             , (core/fn [o prim-call call-form fn-vtable-name]
                 `(~'case (cljscm.core/scm-type-idx (scm* {} ~o))
                    ~@(when (check-impl 'cljscm.core/Number) `[0 ~(prim-call :Number)]) ;Fixnum
                    ~@(when (check-impl 'cljscm.core/Pair) `[3 ~(prim-call :Pair)])
                    ~@(when (some check-impl ['cljscm.core/Boolean 'cljscm.core/Nil 'cljscm.core/Null 'cljscm.core/Char])
                        `[2 (~'case (scm* {} ~o)
                              ~@(when (check-impl 'cljscm.core/Boolean) `[(true false) ~(prim-call :Boolean)])
                              ~@(when (check-impl 'cljscm.core/Nil) `[nil ~(prim-call :Nil)])
                              ~@(when (check-impl 'cljscm.core/Null) `[(scm* [] ()) ~(prim-call :Null)])
                              ~@(when (check-impl 'cljscm.core/Char) `[(when (char? (scm* {} ~o)) ~(prim-call :Char))]))])
                    1 (~'case (cljscm.core/scm-subtype-idx (scm* {} ~o))
                        ~@(when (check-impl 'cljscm.core/Array) `[0 ~(prim-call :Array)])
                        ~@(when (check-impl 'cljscm.core/Number) `[(2 3 30 31) ~(prim-call :Number)]) ;Rational ;Complex ;Flonum, ;Bignum
                        4 ~(call-form `(cljscm.core/scm-table-ref
                                        ~fn-vtable-name
                                        (cljscm.core/scm-unsafe-vector-ref (scm* {} ~o) 0)))
                        ~@(when (check-impl 'cljscm.core/Symbol) `[8 ~(prim-call :Symbol)])
                        ~@(when (check-impl 'cljscm.core/Keyword) `[9 ~(prim-call :Keyword)])
                        ~@(when (check-impl 'cljscm.core/Procedure) `[14 ~(prim-call :Procedure)])
                        ~@(when (check-impl 'cljscm.core/String) `[19 ~(prim-call :String)])
                        ~@(when (check-impl 'cljscm.core/Array) `[(20 21 22 23 24 25 26 27 28 29) ~(prim-call :Array)]) ;Various numerically-typed arrays
                        )))
             sat-o 'o
             fast-satisfies `(cond ~@(mapcat (core/fn [t] [(scm-instance?* t `(scm* {} ~sat-o)) true])
                                             known-implementing-types)
                                   true ~(mk-prim sat-o (core/fn [_] false) (core/fn [_] false) nil)); FIXME Need table lookup last-resort if unknown, here we just false if not hinted.
             method
             , (core/fn
                 [[fname & sigs]]
                 (core/let [single-sig (gather-polysigs (take-while vector? sigs))
                            [o & rst] single-sig
                            _ (when (= o '&) (throw (Exception. "Can't have polyvariadic protocol methods with no fixed args.")))
                            captd-args (map (core/fn [s] `(scm* {} ~s))
                                            (remove #{'&} rst))
                            fx-a (concat [`(scm* {} ~'list)
                                          `(scm* {} ~o)]
                                         (butlast captd-args))
                            rst-a (last captd-args)
                            variadic? (some #{'&} single-sig)
                            fn-name-sym (gensym fname)
                            fn-vtable-name (symbol (str fname "---vtable"))
                            prim-types [:Number :Pair :Boolean :Nil :Null
                                        :Char :Array :Symbol :Keyword :Procedure :String]
                            prim-fnames (map #(symbol (str fname "---cljscm_core$" (name %))) prim-types)
                            #_prim-checks #_{'cljscm.core/Number 'scm-number?
                                             'cljscm.core/Pair 'pair?
                                             'cljscm.core/Boolean 'boolean?
                                             'cljscm.core/Nil '(eq? nil :TODO)}
                            call-form (core/fn [p-fn]
                                        (if variadic?
                                          #_"capture locals that will be introduced later in scheme."
                                          `(scm* {:p-fn ~p-fn :fixed ~fx-a :rest ~rst-a}
                                                 (~'apply :p-fn (~'append :fixed :rest)))
                                          (concat [p-fn `(scm* {} ~o)]
                                                  (map (core/fn [s] `(scm* {} ~s)) (remove #{'&} rst)))))
                            prim-call
                            , (into {} (map #(vector %1 (call-form %2)) prim-types prim-fnames))
                            test-sym (gensym "type")
                            resolved-name (:name (ana/resolve-var (dissoc &env :locals) fname))
;                            _ (println "Proto name: " p)
                            prim-dispatches ; the fat "else" clause when none of the hints apply. Careful not to extract type from prims -- skip the vtable lookup as we'll then know the type.
                            , (mk-prim o prim-call call-form fn-vtable-name)
                            , #_`(let [~test-sym (type (scm* {} ~o))]
                                   (cond
                                     ~@(mapcat (core/fn [ty sp-fn]
                                                 [`(identical? ~ty ~test-sym),
                                                  (if variadic?
                                                    (concat [`apply sp-fn `(scm* {} ~o)] ;capture locals that will be introduced in scheme.
                                                            (map (core/fn [s] `(scm* {} ~s)) (remove #{'&} rst)))
                                                    (concat [sp-fn `(scm* {} ~o)]
                                                            (map (core/fn [s] `(scm* {} ~s)) (remove #{'&} rst))))])
                                               prim-types specialized-fns)))
                            fast-dispatch `(cond ~@(mapcat (core/fn [t] [(scm-instance?* t `(scm* {} ~o)) (call-form (symbol (str fname "---" (->> (str t) (replace {\. \_ \/ \$}) (apply str)))))])
                                                           known-implementing-types)
                                                 true ~prim-dispatches)]
;                   (println "DISPATCH: "fname"->"resolved-name":"known-implementing-types)
                   `(do
;                      ~@(map (core/fn [sf] `(defn ~sf [& args] (throw (cljscm.core/Error. (str "Protocol/Type pair: " (quote ~sf) " not defined."))))) prim-fnames)
                      (def ~fn-vtable-name ~'(scm* {} (make-table)))
                      ~(list 'scm*
                             {fn-name-sym fname
                              ::prim-dispatches fast-dispatch}
                             (list 'define
                                   (apply list (concat [fn-name-sym]
                                                       [o]
                                                       (map #(core/get {'& '.} % %) rst)))
                                   ::prim-dispatches)))))]
    `(do
       ;(set! ~'*unchecked-if* true)
       (def ~psym (quote ~p))
       (defn ~(with-meta (symbol (str "satisfies?---" psym)) {:tag 'boolean}) [~sat-o] ~fast-satisfies)
       ~@(map method methods)
       ;(set! ~'*unchecked-if* false)
       )))

(defmacro satisfies?
  "Returns true if x satisfies the protocol"
  [psym x]
  (core/let [p (:name (ana/resolve-var (dissoc &env :locals) psym))
             prefix (protocol-prefix p)]
    `(~(symbol (namespace p) (str "satisfies?---" (name p))) ~x)
    #_`(let [tx# (type ~x)]
       (scm* {:tx tx# :p ~p :f false}
             ~'(table-ref (table-ref cljscm.core/protocol-impls :tx)
                          :p :f)))))

(defmacro scm-source-marker []
  (list 'scm* {} (symbol "##source2-marker")))

(defmacro scm-table-ref [table key]
  `(scm* {:table ~table :key ~key} ~(list 'table-ref :table :key)))

(defmacro scm-unsafe-vector-ref [vector idx]
  `(scm* {:vector ~vector :idx ~idx} ~(list (symbol "##vector-ref") :vector :idx)))

(defmacro scm-unsafe-vector-set! [vector idx val]
  `(scm* {:vector ~vector :idx ~idx :val ~val} ~(list (symbol "##vector-set!") :vector :idx :val)))

(defmacro scm-type-idx [x]
  `(scm* {:x ~x} ~(list (symbol "##type") :x ) ))

(defmacro scm-subtype-idx [x]
  `(scm* {:x ~x} ~(list (symbol "##subtype") :x ) ))

(defmacro scm-object->class [x]
  `(scm* {:x ~x} ~(list 'object->class :x ) ))

(defmacro lazy-seq [& body]
  `(new cljscm.core/LazySeq nil false (core/fn [] ~@body) nil))

(defmacro delay [& body]
  "Takes a body of expressions and yields a Delay object that will
  invoke the body only the first time it is forced (with force or deref/@), and
  will cache the result and return it on all subsequent force
  calls."
  `(new cljscm.core/Delay (atom {:done false, :value nil}) (fn [] ~@body)))

(defmacro binding
  "binding => var-symbol init-expr

  Creates new bindings for the (already-existing) vars, with the
  supplied initial values, executes the exprs in an implicit do, then
  re-establishes the bindings that existed before.  The new bindings
  are made in parallel (unlike let); all init-exprs are evaluated
  before the vars are bound to their new values."
  [bindings & body]
  (core/let [names (take-nth 2 bindings)
             qualnames (map #(:name (ana/resolve-var (dissoc &env :locals) %)) names)
             vals (take-nth 2 (drop 1 bindings))
             tempnames (map (comp gensym name) names)
             binds (map vector names vals)
             resets (reverse (map vector names tempnames))]
    (ana/confirm-bindings &env names)
    `(~'(scm* {} parameterize)
      (~'scm* ~(apply hash-map (interleave tempnames vals)) ~(map #(list %1 %2) qualnames tempnames))
      ~@body)))

(defmacro condp
  "Takes a binary predicate, an expression, and a set of clauses.
  Each clause can take the form of either:

  test-expr result-expr

  test-expr :>> result-fn

  Note :>> is an ordinary keyword.

  For each clause, (pred test-expr expr) is evaluated. If it returns
  logical true, the clause is a match. If a binary clause matches, the
  result-expr is returned, if a ternary clause matches, its result-fn,
  which must be a unary function, is called with the result of the
  predicate as its argument, the result of that call being the return
  value of condp. A single default expression can follow the clauses,
  and its value will be returned if no clause matches. If no default
  expression is provided and no clause matches, an
  IllegalArgumentException is thrown."
  {:added "1.0"}

  [pred expr & clauses]
  (core/let [gpred (gensym "pred__")
        gexpr (gensym "expr__")
        emit (core/fn emit [pred expr args]
               (core/let [[[a b c :as clause] more]
                       (split-at (if (= :>> (second args)) 3 2) args)
                       n (count clause)]
                 (cond
                  (= 0 n) `(throw (Error. (str "No matching clause: " ~expr)))
                  (= 1 n) a
                  (= 2 n) `(if (~pred ~a ~expr)
                             ~b
                             ~(emit pred expr more))
                  :else `(if-let [p# (~pred ~a ~expr)]
                           (~c p#)
                           ~(emit pred expr more)))))
        gres (gensym "res__")]
    `(let [~gpred ~pred
           ~gexpr ~expr]
       ~(emit gpred gexpr clauses))))

#_(defmacro case [e & clauses]
  (core/let [default (if (odd? (count clauses))
                  (last clauses)
                  `(throw (js/Error. (core/str "No matching clause: " ~e))))
        assoc-test (core/fn assoc-test [m test expr]
                         (if (contains? m test)
                           (throw (clojure.core/IllegalArgumentException.
                                   (core/str "Duplicate case test constant '"
                                             test "'"
                                             (when (:line &env)
                                               (core/str " on line " (:line &env) " "
                                                         ana/*cljs-file*)))))
                           (assoc m test expr)))
        pairs (reduce (core/fn [m [test expr]]
                        (cond
                         (core/seq? test) (reduce (core/fn [m test]
                                               (core/let [test (if (symbol? test)
                                                            (list 'quote test)
                                                            test)]
                                                 (assoc-test m test expr)))
                                             m test)
                         (symbol? test) (assoc-test m (list 'quote test) expr)
                         :else (assoc-test m test expr)))
                      {} (partition 2 clauses))
        esym (gensym)]
   `(let [~esym ~e]
      (cond
        ~@(mapcat (core/fn [[m c]] `((cljscm.core/= ~m ~esym) ~c)) pairs)
        :else ~default))))

(defmacro try
  "(try expr* catch-clause* finally-clause?)

   Special Form

   catch-clause => (catch protoname name expr*)
   finally-clause => (finally expr*)

  Catches and handles JavaScript exceptions."
  [& forms]
  (core/let [catch? #(and (core/seq? %) (= (first %) 'catch))
        [body catches] (split-with (complement catch?) forms)
        [catches fin] (split-with catch? catches)
        e (gensym "e")]
    (assert (every? #(clojure.core/> (count %) 2) catches) "catch block must specify a prototype and a name")
    (if (core/seq catches)
      `(~'try*
        ~@body
        (catch ~e
            (cond
             ~@(mapcat
                (core/fn [[_ type name & cb]]
                  `[(instance? ~type ~e)
                    (let [~name ~e]
                      ((~'scm* {} ~'continuation-return) cljscm.compiler/unwinding-k
                       (do ~@cb)))])
                catches)
             :else (throw ~e)))
        ~@fin)
      `(~'try*
        ~@body
        ~@fin))))

(defmacro assert
  "Evaluates expr and throws an exception if it does not evaluate to
  logical true."
  ([x]
     (when *assert*
       `(when-not ~x
          (throw (cljscm.core/Error.
                  (cljscm.core/str "Assert failed: " (cljscm.core/pr-str '~x)))))))
  ([x message]
     (when *assert*
       `(when-not ~x
          (throw (cljscm.core/Error.
                  (cljscm.core/str "Assert failed: " ~message "\n" (cljscm.core/pr-str '~x))))))))

(defmacro for
  "List comprehension. Takes a vector of one or more
   binding-form/collection-expr pairs, each followed by zero or more
   modifiers, and yields a lazy sequence of evaluations of expr.
   Collections are iterated in a nested fashion, rightmost fastest,
   and nested coll-exprs can refer to bindings created in prior
   binding-forms.  Supported modifiers are: :let [binding-form expr ...],
   :while test, :when test.

  (take 100 (for [x (range 100000000) y (range 1000000) :while (< y x)]  [x y]))"
  [seq-exprs body-expr]
  (assert-args for
     (vector? seq-exprs) "a vector for its binding"
     (even? (count seq-exprs)) "an even number of forms in binding vector")
  (core/let [to-groups (core/fn [seq-exprs]
                    (reduce (core/fn [groups [k v]]
                              (if (keyword? k)
                                (conj (pop groups) (conj (peek groups) [k v]))
                                (conj groups [k v])))
                            [] (partition 2 seq-exprs)))
        err (core/fn [& msg] (throw (apply str msg)))
        emit-bind (core/fn emit-bind [[[bind expr & mod-pairs]
                                  & [[_ next-expr] :as next-groups]]]
                    (core/let [giter (gensym "iter__")
                          gxs (gensym "s__")
                          do-mod (core/fn do-mod [[[k v :as pair] & etc]]
                                   (cond
                                     (= k :let) `(let ~v ~(do-mod etc))
                                     (= k :while) `(when ~v ~(do-mod etc))
                                     (= k :when) `(if ~v
                                                    ~(do-mod etc)
                                                    (recur (rest ~gxs)))
                                     (keyword? k) (err "Invalid 'for' keyword " k)
                                     next-groups
                                      `(let [iterys# ~(emit-bind next-groups)
                                             fs# (core/seq (iterys# ~next-expr))]
                                         (if fs#
                                           (concat fs# (~giter (rest ~gxs)))
                                           (recur (rest ~gxs))))
                                     :else `(cons ~body-expr
                                                  (~giter (rest ~gxs)))))]
                      `(fn ~giter [~gxs]
                         (lazy-seq
                           (loop [~gxs ~gxs]
                             (when-first [~bind ~gxs]
                               ~(do-mod mod-pairs)))))))]
    `(let [iter# ~(emit-bind (to-groups seq-exprs))]
       (iter# ~(second seq-exprs)))))

(defmacro doseq
  "Repeatedly executes body (presumably for side-effects) with
  bindings and filtering as provided by \"for\".  Does not retain
  the head of the sequence. Returns nil."
  [seq-exprs & body]
  (assert-args doseq
     (vector? seq-exprs) "a vector for its binding"
     (even? (count seq-exprs)) "an even number of forms in binding vector")
  (core/let [step (core/fn step [recform exprs]
               (if-not exprs
                 [true `(do ~@body)]
                 (core/let [k (first exprs)
                       v (second exprs)

                       seqsym (when-not (keyword? k) (gensym))
                       recform (if (keyword? k) recform `(recur (next ~seqsym)))
                       steppair (step recform (nnext exprs))
                       needrec (steppair 0)
                       subform (steppair 1)]
                   (cond
                     (= k :let) [needrec `(let ~v ~subform)]
                     (= k :while) [false `(when ~v
                                            ~subform
                                            ~@(when needrec [recform]))]
                     (= k :when) [false `(if ~v
                                           (do
                                             ~subform
                                             ~@(when needrec [recform]))
                                           ~recform)]
                     :else [true `(loop [~seqsym (core/seq ~v)]
                                    (when ~seqsym
                                      (let [~k (first ~seqsym)]
                                        ~subform
                                        ~@(when needrec [recform]))))]))))]
    (nth (step nil (core/seq seq-exprs)) 1)))

(defmacro aclone
  "Returns a javascript array, cloned from the passed in array"
  ([a]
     `(scm* {:a ~a} ~'(vector-copy :a)))
  ([a sz]
     `(let [sz# ~sz
            r# (scm* {:sz sz#} ~'(make-vector :sz))]
        (scm* {:a ~a :sz sz# :r r#}
              ~'(subvector-move! :a 0 :sz :r 0))
        r#)))

(defmacro aclone-push
  "Clones the array into an array one element larger; with the value pushed."
  [a o]
   `(let [sz# (scm* {:a ~a} ~'(vector-length :a))
         r# (scm* {:nsz (inc sz#)} ~'(make-vector :nsz))]
      (scm* {:a ~a :sz sz# :r r# :o ~o}
            ~'(begin
               (subvector-move! :a 0 :sz :r 0)
               (vector-set! :r :sz :o)))
      r#))

(defmacro aclone-push2
  "Clones the array into a new array of size (+ sz2);
   with the values pushed after sz."
  [a sz o1 o2]
   `(let [sz# ~sz
         r# (scm* {:nsz (+ sz# 2)} ~'(make-vector :nsz))]
      (scm* {:a ~a :sz sz# :r r# :o1 ~o1 :o2 ~o2}
            ~'(begin
               (subvector-move! :a 0 :sz :r 0)
               (vector-set! :r :sz :o1)
               (vector-set! :r (+ :sz 1) :o2)))
      r#))

(defmacro aclone-padded
  "Clones array a into a new array of size sz, padded with nil."
  [a sz]
   `(let [asz# (scm* {:a ~a} ~'(vector-length :a))
          r# (scm* {:sz ~sz :nil nil} ~'(make-vector :sz :nil))]
      (scm* {:a ~a :asz asz# :r r#}
            ~'(begin
               (subvector-move! :a 0 :asz :r 0)))
      r#))

(defmacro slice
  "does NOT re-create Javascripts negative index sematics, just end >= start slicing. Also, will complain on out-of-ranges; won't be clipped to viable length."
  [a start end]
  `(let [sz# (- ~end ~start)
         r# (scm* {:sz sz#} ~'(make-vector :sz))]
     (scm* {:a ~a :start ~start :end ~end :r r#} ~'(subvector-move! :a :start :end :r 0))
     r#))

(defmacro slice-pop
  "returns a copy without the last element"
  [a]
  `(let [sz# (dec (alength ~a))
         r# (scm* {:sz sz#} ~'(make-vector :sz))]
     (scm* {:a ~a :r r# :sz sz#} ~'(subvector-move! :a 0 :sz :r 0))
     r#))

(defmacro amap
  "Maps an expression across an array a, using an index named idx, and
  return value named ret, initialized to a clone of a, then setting
  each element of ret to the evaluation of expr, returning the new
  array ret."
  [a idx ret expr]
  `(let [a# ~a
         ~ret (aclone a#)]
     (loop  [~idx 0]
       (if (< ~idx  (alength a#))
         (do
           (aset ~ret ~idx ~expr)
           (recur (inc ~idx)))
         ~ret))))

(defmacro areduce
  "Reduces an expression across an array a, using an index named idx,
  and return value named ret, initialized to init, setting ret to the
  evaluation of expr at each step, returning ret."
  [a idx ret init expr]
  `(let [a# ~a]
     (loop  [~idx 0 ~ret ~init]
       (if (< ~idx  (alength a#))
         (recur (inc ~idx) ~expr)
         ~ret))))

(defmacro dotimes
  "bindings => name n

  Repeatedly executes body (presumably for side-effects) with name
  bound to integers from 0 through n-1."
  [bindings & body]
  (core/let [i (first bindings)
        n (second bindings)]
    `(let [n# ~n]
       (loop [~i 0]
         (when (< ~i n#)
           ~@body
           (recur (inc ~i)))))))

(core/defn ^:private check-valid-options
  "Throws an exception if the given option map contains keys not listed
  as valid, else returns nil."
  [options & valid-keys]
  (when (core/seq (apply disj (apply hash-set (keys options)) valid-keys))
    (throw
     (apply core/str "Only these options are valid: "
	    (first valid-keys)
	    (map #(core/str ", " %) (rest valid-keys))))))

(defmacro defmulti
  "Creates a new multimethod with the associated dispatch function.
  The docstring and attribute-map are optional.

  Options are key-value pairs and may be one of:
    :default    the default dispatch value, defaults to :default
    :hierarchy  the isa? hierarchy to use for dispatching
                defaults to the global hierarchy"
  [mm-name & options]
  (core/let [docstring   (if (core/string? (first options))
                      (first options)
                      nil)
        options     (if (core/string? (first options))
                      (next options)
                      options)
        m           (if (map? (first options))
                      (first options)
                      {})
        options     (if (map? (first options))
                      (next options)
                      options)
        dispatch-fn (first options)
        options     (next options)
        m           (if docstring
                      (assoc m :doc docstring)
                      m)
        m           (if (meta mm-name)
                      (conj (meta mm-name) m)
                      m)]
    (when (= (count options) 1)
      (throw "The syntax for defmulti has changed. Example: (defmulti name dispatch-fn :default dispatch-value)"))
    (core/let [options   (apply hash-map options)
          default   (core/get options :default :default)]
      (check-valid-options options :default :hierarchy)
      `(def ~(with-meta mm-name m)
         (let [method-table# (atom {})
               prefer-table# (atom {})
               method-cache# (atom {})
               cached-hierarchy# (atom {})
               hierarchy# (get ~options :hierarchy cljscm.core/global-hierarchy)]
           (cljscm.core/MultiFn. ~(name mm-name) ~dispatch-fn ~default hierarchy#
                               method-table# prefer-table# method-cache# cached-hierarchy#))))))

(defmacro defmethod
  "Creates and installs a new method of multimethod associated with dispatch-value. "
  [multifn dispatch-val & fn-tail]
  `(-add-method ~(with-meta multifn {:tag 'cljscm.core/MultiFn}) ~dispatch-val (core/fn ~@fn-tail)))

(defmacro time
  "Evaluates expr and prints the time it took. Returns the value of expr."
  [expr]
  `(let [start# (.getTime (js/Date.))
         ret# ~expr]
     (prn (core/str "Elapsed time: " (- (.getTime (js/Date.)) start#) " msecs"))
     ret#))

(defmacro simple-benchmark
  "Runs expr iterations times in the context of a let expression with
  the given bindings, then prints out the bindings and the expr
  followed by number of iterations and total time. The optional
  argument print-fn, defaulting to println, sets function used to
  print the result. expr's string representation will be produced
  using pr-str in any case."
  [bindings expr iterations & {:keys [print-fn] :or {print-fn 'println}}]
  (core/let [bs-str   (pr-str bindings)
        expr-str (pr-str expr)]
    `(let ~bindings
       (let [start#   (.getTime (js/Date.))
             ret#     (dotimes [_# ~iterations] ~expr)
             end#     (.getTime (js/Date.))
             elapsed# (- end# start#)]
         (~print-fn (str ~bs-str ", " ~expr-str ", "
                         ~iterations " runs, " elapsed# " msecs"))))))

(def cs (into [] (map (comp symbol core/str char) (range 97 118))))

(core/defn gen-apply-to-helper
  ([] (gen-apply-to-helper 1))
  ([n]
     (core/let [prop (symbol (core/str "-cljs$lang$arity$" n))
           f (symbol (core/str "cljs$lang$arity$" n))]
       (if (core/<= n 20)
         `(let [~(cs (core/dec n)) (-first ~'args)
                ~'args (-rest ~'args)]
            (if (core/== ~'argc ~n)
              (if (. ~'f ~prop)
                (. ~'f (~f ~@(take n cs)))
                (~'f ~@(take n cs)))
              ~(gen-apply-to-helper (core/inc n))))
         `(throw (js/Error. "Only up to 20 arguments supported on functions"))))))

(defmacro gen-apply-to []
  `(do
     (set! ~'*unchecked-if* true)
     (defn ~'apply-to [~'f ~'argc ~'args]
       (let [~'args (core/seq ~'args)]
         (if (zero? ~'argc)
           (~'f)
           ~(gen-apply-to-helper))))
     (set! ~'*unchecked-if* false)))

(defmacro with-out-str
  "Evaluates exprs in a context in which *print-fn* is bound to .append
  on a fresh StringBuffer.  Returns the string created by any nested
  printing calls."
  [& body]
  `(let [sb# (string-buffer-writer)]
     (binding [cljscm.core/*print-fn* (core/fn [x#] (-write sb# x#))]
       ~@body)
     (-toString sb#)))

(defmacro eof-object? [o]
  `(scm-boolean* {::o ~o} ~'(eof-object? ::o)))
