;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.core
  (:refer-clojure :exclude [-> ->> .. amap and areduce alength aclone assert binding bound-fn case comment cond condp
                            declare definline definterface defmethod defmulti defn defn- defonce
                            defprotocol defrecord defstruct deftype delay destructure doseq dosync dotimes doto
                            extend-protocol extend-type fn for future gen-class gen-interface
                            if-let if-not import io! lazy-cat lazy-seq let letfn locking loop
                            memfn ns or proxy proxy-super pvalues refer-clojure reify sync time
                            when when-first when-let when-not while with-bindings with-in-str
                            with-loading-context with-local-vars with-open with-out-str with-precision with-redefs
                            satisfies? identical? true? false? nil? get

                            aget aset
                            + - * / < <= > >= == zero? pos? neg? inc dec max min mod quot rem 
                            bit-and bit-and-not bit-clear bit-flip bit-not bit-or bit-set
                            bit-test bit-shift-left bit-shift-right bit-xor])
  (:require clojure.walk))

(alias 'core 'clojure.core)

(defmacro import-macros [ns [& vars]]
  (core/let [ns (find-ns ns)
             vars (map #(ns-resolve ns %) vars)
             syms (map (core/fn [^clojure.lang.Var v] (core/-> v .sym (with-meta {:macro true}))) vars)
             defs (map (core/fn [sym var]
                                `(def ~sym (deref ~var))) syms vars)]
            `(do ~@defs
                 :imported)))

(import-macros clojure.core
 [-> ->> ..  and assert comment cond
  declare defn defn-
  doto
  extend-protocol fn for
  if-let if-not letfn
  memfn or 
  when when-first when-let when-not while])

(def ^:dynamic *cljs-ns* 'cljs.user)

(def compare-arglist
  (comparator
   (fn* [arglist-a arglist-b]
     (if (some #{'&} arglist-a)
       false
       (if (some #{'&} arglist-b)
         true
         (core/< (count arglist-a) (count arglist-b)))))))

(defmacro js* [& forms])
(defmacro scm-str* [& forms]
  `(~'scm-str* ~@forms))
(defmacro scm* [& forms]
    `(~'scm* ~@forms))

(defn
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

(defmacro ^{:private true} assert-args [fnname & pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                  ~(core/str fnname " requires " (second pairs)))))
     ~(core/let [more (nnext pairs)]
        (when more
          (list* `assert-args fnname more)))))

(defn destructure [bindings]
  (core/let [bents (partition 2 bindings)
         pb (fn pb [bvec b v]
              (core/let [pvec
                     (fn [bvec b val]
                       (core/let [gvec (gensym "vec__")]
                         (core/loop [ret (-> bvec (conj gvec) (conj val))
                                     n 0
                                     bs b
                                     seen-rest? false]
                           (if (seq bs)
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
                     (fn [bvec b v]
                       (core/let [gmap (gensym "map__")
                                  defaults (:or b)]
                         (core/loop [ret (-> bvec (conj gmap) (conj v)
                                             (conj gmap) (conj `(if (seq? ~gmap) (apply hash-map ~gmap) ~gmap))
                                             ((fn [ret]
                                                (if (:as b)
                                                  (conj ret (:as b) gmap)
                                                  ret))))
                                     bes (reduce
                                          (fn [bes entry]
                                            (reduce #(assoc %1 %2 ((val entry) %2))
                                                    (dissoc bes (key entry))
                                                    ((key entry) bes)))
                                          (dissoc b :as :or)
                                          {:keys #(keyword (core/str %)), :strs core/str, :syms #(list `quote %)})]
                           (if (seq bes)
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
         process-entry (fn [bvec b] (pb bvec (first b) (second b)))]
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
              gs (map (fn [b] (if (symbol? b) b (gensym))) bs)
              bfs (reduce (fn [ret [b v g]]
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
  (zipmap (map #(symbol "cljs.core" (core/str %))
               '[IFn ICounted IEmptyableCollection ICollection IIndexed ASeq ISeq INext
                 ILookup IAssociative IMap IMapEntry ISet IStack IVector IDeref
                 IDerefWithTimeout IMeta IWithMeta IReduce IKVReduce IEquiv IHash
                 ISeqable ISequential IList IRecord IReversible ISorted IPrintable IWriter
                 IPrintWithWriter IPending IWatchable IEditableCollection ITransientCollection
                 ITransientAssociative ITransientMap ITransientVector ITransientSet
                 IMultiFn IChunkedSeq IChunkedNext IComparable])
          (iterate (fn [[p b]]
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

(defn bool-expr [e]
  (vary-meta e assoc :tag 'boolean))

(defmacro nil? [x]
  `(coercive-= ~x nil))

;; internal - do not use.
(defmacro coercive-not [x]
   (bool-expr `(scm* {:x ~x}
                     ~'(not :x))))

;; internal - do not use.
(defmacro coercive-not= [x y]
  (bool-expr `(scm* {:x ~x :y ~y}
                    ~'(not (eqv? :x :y)))))

;; internal - do not use.
(defmacro coercive-= [x y]
  (bool-expr `(scm* {:x ~x :y ~y}
                    ~'(equal? :x :y))))

(defmacro true? [x]
  (bool-expr `(identical? ~x true)))

(defmacro false? [x]
  (bool-expr `(identical? ~x false)))

(defmacro undefined? [x]
  `(nil? ~x))
  
(defmacro identical? [a b]
  `(scm* {:a ~a :b ~b}
         ~'(eqv? :a :b)))

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
             combined-sigs
             , (vec (if (core/<= (count dsigs) 1)
                      (if any-variadic?
                        (let [args (first (first dsigs))
                              body (second (first dsigs))]
                          [args `((fn [~@(drop-last 2 args) ~(last args)] ~body)
                                  ~@(drop-last 2 args) ~(last args))])
                        dsigs)
                      (core/let [[smallest-sig & other-sigs] (sort-by first compare-arglist dsigs)
                                 middle-sigs (butlast other-sigs)
                                 biggest-sig (last other-sigs)
                                 restparam (gensym "rest")
                                 bind-rst
                                 , (fn* [sig]
                                        (core/let
                                            [variadic? (some #{'&} (first sig))
                                             fixed-args (if variadic?
                                                          (butlast (butlast
                                                                    (drop (count (first smallest-sig))
                                                                          (first sig))))
                                                          (drop (count (first smallest-sig))
                                                                (first sig)))
                                             rest-arg (when variadic? (last (first sig)))
                                             args-inits (concat
                                                         (filter identity
                                                                 (map (fn [s m] (when true #_(not= s m) [m s])) ;alias small arg names
                                                                      (first smallest-sig) (first sig)))
                                                         (map (fn [i a] [a `(~'scm* [~restparam] (~'list-ref ~restparam ~i))])
                                                              (range) fixed-args)
                                                         (when rest-arg [[rest-arg
                                                                          `(~'scm* [~restparam] ~(nth (iterate #(list 'cdr %) restparam) (count fixed-args)))]]))]
                                          `((fn [~@(map first args-inits)]
                                              ~@(rest sig))
                                            ~@(map second args-inits))))]
                        `(~(vec (concat (first smallest-sig) ['& restparam])) 
                          (~'case (count ~restparam)
                            0 ((fn [~@(first smallest-sig)] ~@(rest smallest-sig)) ~@(first smallest-sig))
                            ~@(apply concat
                                     (map-indexed
                                      (fn* [i sig] [(clojure.core/inc i) (bind-rst sig)])
                                      middle-sigs))
                            ~@(when biggest-sig
                                (if any-variadic?
                                  [(bind-rst biggest-sig)]
                                  [(clojure.core/inc (count middle-sigs)) (bind-rst biggest-sig)
                                   `(throw (cljs.core.Error. (str "Wrong number of args: (" (+ ~(count (first smallest-sig)) (count ~restparam)) ")")))])))))))]
    (with-meta
      (if name
        (list* 'fn* name combined-sigs)
        (cons 'fn* combined-sigs))
      (meta &form))))

(defmacro aget
  ([a i]
     `(scm* {:a ~a :i ~i}
            ~'(vector-ref :a :i)))
  ([a i & idxs]
     `(aget (aget ~a ~i) ~@idxs)))

(defmacro aset [a i v]
  `(let [ra# ~a]
     (scm* {:ra ra# :i ~i :v ~v}
           ~'(vector-set! :ra :i :v))
     ~v))

(defmacro +
  ([& more] `(scm* ~more ~(cons '+ more))))

(defmacro -
  ([& more] `(scm* ~more ~(cons '- more))))

(defmacro *
  ([& more] `(scm* ~more ~(cons '* more))))

(defmacro /
  ([& more] `(scm* ~more ~(cons '/ more))))

(defmacro <
  ([& more] (bool-expr `(scm* ~more ~(cons '< more)))))

(defmacro <=
  ([& more] (bool-expr `(scm* ~more ~(cons '<= more)))))

(defmacro >
  ([& more] (bool-expr `(scm* ~more ~(cons '> more)))))

(defmacro >=
  ([& more] (bool-expr `(scm* ~more ~(cons '>= more)))))

(defmacro ==
  ([& more] (bool-expr `(scm* ~more ~(cons '= more)))))

(defmacro dec [x]
  `(- ~x 1))

(defmacro inc [x]
  `(+ ~x 1))

(defmacro zero? [x]
  `(== ~x 0))

(defmacro pos? [x]
  `(scm* {:x ~x} ~'(positive? :x)))

(defmacro neg? [x]
  `(scm* {:x ~x} ~'(negative? :x)))

(defmacro max
  ([& more] `(scm* ~more ~(cons 'max more))))

(defmacro min
  ([& more] `(scm* ~more ~(cons 'min more))))

(defmacro mod [num div]
  `(scm* {:num ~num :div ~div} ~'(if (or (flonum? :num) (flonum? :div))
                                   (- :num (* :div (fltruncate (/ :num :div))))
                                   (modulo :num :div))))

(defmacro quot [num div]
  `(scm* {:num ~num :div ~div} ~'(if (or (flonum? :num) (flonum? :div))
                                   (- :num (* :div (fltruncate (/ :num :div))))
                                   (quotient :num :div))))

(defmacro rem [num div]
  `(scm* {:num ~num :div ~div} ~'(if (or (flonum? :num) (flonum? :div))
                                   (- :num (* :div (fltruncate (/ :num :div))))
                                   (remainder :num :div))))

(defmacro bit-not [x]
  `(scm* {:x ~x} ~'(bitwise-not :x)))

(defmacro bit-and
  ([& more] `(scm* ~more ~(cons 'bitwise-and more))))

;; internal do not use
(defmacro unsafe-bit-and
  ([x y] (bool-expr (list 'js* "(~{} & ~{})" x y)))
  ([x y & more] `(unsafe-bit-and (unsafe-bit-and ~x ~y) ~@more)))

(defmacro bit-or
  ([& more] `(scm* ~more ~(cons 'bitwise-ior more))))

(defmacro bit-xor
  ([& more] `(scm* ~more ~(cons 'bitwise-xor more))))

(defmacro bit-and-not
  [x y]
  `(bit-and ~x (bit-not ~y)))

#_(defmacro bit-clear [x n]
  (list 'js* "(~{} & ~(1 << ~{}))" x n))

#_(defmacro bit-flip [x n]
  (list 'js* "(~{} ^ (1 << ~{}))" x n))

#_(defmacro bit-test [x n]
  (list 'js* "((~{} & (1 << ~{})) != 0)" x n))

#_(defmacro bit-shift-left [x n]
  (list 'js* "(~{} << ~{})" x n))

#_(defmacro bit-shift-right [x n]
  (list 'js* "(~{} >> ~{})" x n))

#_(defmacro bit-set [x n]
    (list 'js* "(~{} | (1 << ~{}))" x n))

#_(defmacro bit-shift-right-zero-fill [x n]
    (list 'js* "(~{} >>> ~{})" x n))

;; internal
(defmacro mask [hash shift]
  (list 'js* "((~{} >>> ~{}) & 0x01f)" hash shift))

;; internal
(defmacro bitpos [hash shift]
  (list 'js* "(1 << ~{})" `(mask ~hash ~shift)))

;; internal
(defmacro caching-hash [coll hash-fn hash-key]
  `(let [h# ~hash-key]
     (if-not (nil? h#)
       h#
       (let [h# (~hash-fn ~coll)]
         (set! ~hash-key h#)
         h#))))

(defmacro get
  ([coll k]
     `(-lookup ~coll ~k nil))
  ([coll k not-found]
     `(-lookup ~coll ~k ~not-found)))

;;; internal -- reducers-related macros

(defn- do-curried
  [name doc meta args body]
  (core/let [cargs (vec (butlast args))]
    `(defn ~name ~doc ~meta
       (~cargs (fn [x#] (~name ~@cargs x#)))
       (~args ~@body))))

(defmacro ^:private defcurried
  "Builds another arity of the fn that returns a fn awaiting the last
  param"
  [name doc meta args & body]
  (do-curried name doc meta args body))

(defn- do-rfn [f1 k fkv]
  `(fn
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

(defn protocol-prefix [psym]
  (core/str (-> (core/str psym) (.replace \. \$) (.replace \/ \$)) "$"))

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
        munge  cljs.compiler/munge
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

#_(defn to-property [sym]
  (symbol (core/str "-" sym)))

(defn gather-polysigs
  "[[x y][x y z]] -> [x y & rst]"
  [sigs]
  (core/let [smallest-sig (first (sort-by count sigs))]
    (vec (concat smallest-sig
            (if (clojure.core/> (count sigs) 1) ['& 'rst] [])))))

(defn- parse-impls [specs]
  (core/loop [ret {} s specs]
    (if (seq s)
      (recur (assoc ret (first s) (take-while seq? (next s)))
             (drop-while seq? (next s)))
      ret)))

;It seems this clobbers multi-variadic protocol methods?
(defn- emit-hinted-impl [c [p fs]]
  (core/let [hint (fn [specs]
               (core/let [specs (if (vector? (first specs)) 
                             (list specs) 
                             specs)]
                 (map (core/fn [[[target & args] & body]]
                               (cons (apply vector (vary-meta target assoc :tag c) args)
                                     body))
                      specs)))]
    [p
     (apply merge-with (fn [old new] (concat old (drop 2 new)))
            (map (fn [[f & r :as fr]] {(-> f name keyword)
                              (with-meta (cons 'fn (cons f (hint r))) (assoc (meta fr) :protocol-impl true))})
                 fs))
     #_(zipmap (map #(-> % first name keyword) fs)
               (map #(cons 'fn (hint (drop 1 %))) fs))]))

(defn- emit-extend-type [c specs]
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
  (core/let [resolve #(core/let [ret (:name (cljs.analyzer/resolve-var (dissoc &env :locals) %))]
                   (assert ret (core/str "Can't resolve: " %))
                   ret)
        impl-map (core/loop [ret {} s impls]
                   (if (seq s)
                     (recur (assoc ret (first s) (take-while seq? (next s)))
                            (drop-while seq? (next s)))
                     ret))
        warn-if-not-protocol #(when-not (= 'Object %)
                                (if cljs.analyzer/*cljs-warn-on-undeclared*
                                  (if-let [var (cljs.analyzer/resolve-existing-var (dissoc &env :locals) %)]
                                    (do
                                     (when-not (:protocol-symbol var)
                                       (cljs.analyzer/warning &env
                                         (core/str "WARNING: Symbol " % " is not a protocol")))
                                     (when (and cljs.analyzer/*cljs-warn-protocol-deprecated*
                                                (-> var :deprecated)
                                                (not (-> % meta :deprecation-nowarn)))
                                       (cljs.analyzer/warning &env
                                         (core/str "WARNING: Protocol " % " is deprecated"))))
                                    (cljs.analyzer/warning &env
                                      (core/str "WARNING: Can't resolve protocol symbol " %)))))
        skip-flag (set (-> tsym meta :skip-protocol-flag))]
    (if (base-type tsym)
      (core/let [t (base-type tsym)
                        assign-impls (fn [[p sigs]]
                           (warn-if-not-protocol p)
                           (core/let [psym (resolve p)
                                 pfn-prefix (subs (core/str psym) 0 (clojure.core/inc (.indexOf (core/str psym) "/")))]
                             (cons `(aset ~psym ~t true)
                                   (map (fn [[f & meths :as form]]
                                          `(aset ~(symbol (core/str pfn-prefix f)) ~t ~(with-meta `(fn ~@meths) (meta form))))
                                        sigs))))]
        `(do ~@(mapcat assign-impls impl-map)))
      (core/let [t (resolve tsym)
            prototype-prefix (fn [sym]
                               `(.. ~tsym -prototype ~(to-property sym)))
            assign-impls (fn [[p sigs]]
                           (warn-if-not-protocol p)
                           (core/let [psym (resolve p)
                                 pprefix (protocol-prefix psym)]
                             (if (= p 'Object)
                               (core/let [adapt-params (fn [[sig & body]]
                                                    (core/let [[tname & args] sig]
                                                      (list (vec args) (list* 'this-as (vary-meta tname assoc :tag t) body))))]
                                 (map (fn [[f & meths :as form]]
                                        `(set! ~(prototype-prefix f)
                                               ~(with-meta `(fn ~@(map adapt-params meths)) (meta form))))
                                      sigs))
                               (concat (when-not (skip-flag psym)
                                         [`(set! ~(prototype-prefix pprefix) true)])
                                       (mapcat (fn [[f & meths :as form]]
                                                 (if (= psym 'cljs.core/IFn)
                                                   (core/let [adapt-params (fn [[[targ & args :as sig] & body]]
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
                                                         adapt-params (fn [[[targ & args :as sig] & body]]
                                                                        (cons (vec (cons (vary-meta targ assoc :tag t) args))
                                                                              body))]
                                                     (if (vector? (first meths))
                                                       [`(set! ~(prototype-prefix (core/str pf "$arity$" (count (first meths)))) ~(with-meta `(fn ~@(adapt-params meths)) (meta form)))]
                                                       (map (fn [[sig & body :as meth]]
                                                              `(set! ~(prototype-prefix (core/str pf "$arity$" (count sig)))
                                                                     ~(with-meta `(fn ~(adapt-params meth)) (meta form))))
                                                            meths)))))
                                               sigs)))))]
        `(do ~@(mapcat assign-impls impl-map))))))

(defn- prepare-protocol-masks [env t impls]
  (core/let [resolve #(core/let [ret (:name (cljs.analyzer/resolve-var (dissoc env :locals) %))]
                   (assert ret (core/str "Can't resolve: " %))
                   ret)
        impl-map (core/loop [ret {} s impls]
                   (if (seq s)
                     (recur (assoc ret (first s) (take-while seq? (next s)))
                            (drop-while seq? (next s)))
                     ret))]
    (if-let [fpp-pbs (seq (keep fast-path-protocols
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

(defn dt->et
  ([t specs fields] (dt->et t specs fields false))
  ([t specs fields inline]
     (letfn [(inline-arity? [sig] (not (vector? (second sig))))]
       (core/loop [ret [] s specs]
         (if (seq s)
           (recur (-> ret
                      (conj (first s))
                      (into
                       (reduce (core/fn [v [f sigs]]
                                 (conj v (vary-meta (cons f (mapcat #(if (inline-arity? %)
                                                                       (drop 1 %)
                                                                       (list (drop 1 %))) sigs))
                                                    assoc :cljs.analyzer/type t
                                                    :cljs.analyzer/fields fields
                                                    :protocol-impl true
                                                    :protocol-inline inline)))
                               []
                               (group-by first (take-while seq? (next s))))))
                  (drop-while seq? (next s)))
           ret)))))

(defn collect-protocols [impls env]
  (->> impls
      (filter symbol?)
      (map #(:name (cljs.analyzer/resolve-var (dissoc env :locals) %)))
      (into #{})))

(defmacro deftype [t fields & impls]
  (core/let [r (:name (cljs.analyzer/resolve-var (dissoc &env :locals) t))
        [fpps pmasks] (prepare-protocol-masks &env t impls)
        protocols (collect-protocols impls &env)
        t (vary-meta t assoc
            :protocols protocols
            :skip-protocol-flag fpps) ]
    (if (seq impls)
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

(defn- emit-defrecord
  "Do not use this directly - use defrecord"
  [env tagname rname fields impls]
  (core/let [hinted-fields fields
        fields (vec (map #(with-meta % nil) fields))
        base-fields fields
        #_"depends on Gambit's record lookup i.e. cljs.core/MyRecord-__meta"
        accessorize (fn [this fld] `(~(symbol (str tagname "-" fld)) ~this))
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
                  `(~'-seq [~gthis] (seq (concat [~@(map #(list `vector (keyword %) (accessorize gthis %)) base-fields)] 
                                                 ~(accessorize gthis '__extmap))))
                  
                  'IPrintWithWriter
                  `(~'-pr-writer [gthis writer# opts#]
                                 (let [pr-pair# (fn [keyval#] (pr-sequential-writer writer# pr-writer "" " " "" opts# keyval#))]
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
          extended-constructor-name (symbol (str "make-" *cljs-ns* "/" tagname "-extended"))
          poly-constructor-name (symbol (str "make-" *cljs-ns* "/" tagname))
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
         (~'scm* {::tagname ~tagname} (~'table-set! ~'cljs.core/protocol-impls ::tagname ~'(make-table)))
         (extend-type ~tagname ~@(dt->et tagname impls fields true))))))

(defn- build-positional-factory
  [rsym rname fields]
  (core/let [fn-name (symbol (core/str '-> rsym))]
    `(defn ~fn-name
       [~@fields]
       (new ~rname ~@fields))))

(defn- build-map-factory
  [rsym rname fields]
  (core/let [fn-name (symbol (core/str 'map-> rsym))
	ms (gensym)
	ks (map keyword fields)
	getters (map (core/fn [k] `(~k ~ms)) ks)]
    `(defn ~fn-name
       [~ms]
       (new ~rname ~@getters nil (dissoc ~ms ~@ks)))))

(defmacro defrecord [rsym fields & impls]
  (core/let [r (:name (cljs.analyzer/resolve-var (dissoc &env :locals) rsym))]
    `(do
       ~(emit-defrecord &env rsym r fields impls)
       (set! (.-cljs$lang$ctorPrSeq ~r) (fn [this#] (list ~(core/str r))))
       (set! (.-cljs$lang$ctorPrWriter ~r) (fn [this# writer#] (-write writer# ~(core/str r))))
       ~(build-positional-factory rsym r fields)
       ~(build-map-factory rsym r fields)
       ~r)))

(defn gather-defns
  "([x] ..) ([x y] ..) -> ([x & rest] case-on-arg-count...)"
  [methods]
  (if (= 1 (count methods))
    `(defn ~methods)
    (let [sig (gather-polysigs (map first methods))]
      `(defn ~(gather-polysigs (map first methods))
         (case (clojure.core/count ~(last sig)
                                   ~(core/- (count sig) 2)) (rest (first methods)))))))

(defmacro defprotocol [psym & doc+methods]
  (core/let [p (:name (cljs.analyzer/resolve-var (dissoc &env :locals) psym))
        psym (vary-meta psym assoc :protocol-symbol true)
        ns-name (-> &env :ns :name)
        fqn (core/fn [n] (symbol (str ns-name "." n)))
        prefix (protocol-prefix p)
        methods (if (string? (first doc+methods)) (next doc+methods) doc+methods)
        expand-sig (fn [fname slot sig]
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
        #_method #_(fn [[fname & sigs]]
                     (core/let [sigs (take-while vector? sigs)
                           slot (symbol (core/str prefix (name fname)))
                           fname (vary-meta fname assoc :protocol p)]
                       `(defn ~fname ~@(map (fn [sig]
                                              (expand-sig fname
                                                          (symbol (core/str slot "$arity$" (count sig)))
                                                          sig))
                                            sigs))))
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
                 prim-fnames (map #(symbol (str fname "---cljs_core$" (name %))) prim-types)
                 call-form (fn [p-fn]
                             (if variadic?
                               #_"capture locals that will be introduced later in scheme."
                               `(scm* {:p-fn ~p-fn :fixed ~fx-a :rest ~rst-a}
                                      (~'apply :p-fn (~'append :fixed :rest)))
                               (concat [p-fn `(scm* {} ~o)]
                                       (map (fn [s] `(scm* {} ~s)) (remove #{'&} rst)))))
                 prim-call
                 , (into {} (map #(vector %1 (call-form %2)) prim-types prim-fnames))
                 test-sym (gensym "type")
                 prim-dispatches
                 , `(~'case (cljs.core/scm-type-idx (scm* {} ~o))
                      0 ~(prim-call :Number) ;Fixnum
                      3 ~(prim-call :Pair)   
                      2 (~'case (scm* {} ~o)
                          (true false) ~(prim-call :Boolean)
                          nil ~(prim-call :Nil)
                          (scm* [] ()) ~(prim-call :Null)
                          (when (char? (scm* {} ~o)) ~(prim-call :Char)))
                      1 (~'case (cljs.core/scm-subtype-idx (scm* {} ~o))
                          0 ~(prim-call :Array)
                          (2 3 30 31) ~(prim-call :Number) ;Rational ;Complex ;Flonum, ;Bignum
                          4 ~(call-form `(cljs.core/scm-table-ref
                                          ~fn-vtable-name
                                          (cljs.core/scm-unsafe-vector-ref (scm* {} ~o) 0)))
                          8 ~(prim-call :Symbol)
                          9 ~(prim-call :Keyword)
                          14 ~(prim-call :Procedure)
                          19 ~(prim-call :String)
                          (20 21 22 23 24 25 26 27 28 29) ~(prim-call :Array) ;Various numerically-typed arrays
                          ))
                 , #_`(let [~test-sym (type (scm* {} ~o))]
                        (cond
                          ~@(mapcat (fn [ty sp-fn]
                                      [`(identical? ~ty ~test-sym),
                                       (if variadic?
                                         (concat [`apply sp-fn `(scm* {} ~o)] ;capture locals that will be introduced in scheme.
                                                 (map (fn [s] `(scm* {} ~s)) (remove #{'&} rst)))
                                         (concat [sp-fn `(scm* {} ~o)]
                                                 (map (fn [s] `(scm* {} ~s)) (remove #{'&} rst))))])
                                    prim-types specialized-fns)))]
             `(do
                ~@(map (fn [sf] `(defn ~sf [& args] (throw (cljs.core/Error. (str "Protocol/Type pair: " (quote ~sf) " not defined."))))) prim-fnames)
                (def ~fn-vtable-name {})
                ~(list 'scm*
                       {fn-name-sym fname
                        ::prim-dispatches prim-dispatches}
                       (list 'define
                             (apply list (concat [fn-name-sym]
                                                 [o]
                                                 (map #(core/get {'& '.} % %) rst)))
                             ::prim-dispatches)))))]
    `(do
       (set! ~'*unchecked-if* true)
       (def ~psym (quote ~p))
       ~@(map method methods)
       (set! ~'*unchecked-if* false))))

(defmacro satisfies?
  "Returns true if x satisfies the protocol"
  [psym x]
  (core/let [p (:name (cljs.analyzer/resolve-var (dissoc &env :locals) psym))
        prefix (protocol-prefix p)]
    `(let [tx# (type ~x)]
       (scm* {:tx tx# :p ~p :f false}
             ~'(with-exception-catcher
                 (lambda (e) (if (unbound-table-key-exception? e)
                             :f
                             (raise e)))
                 (lambda () (table-ref (table-ref cljs.core/protocol-impls :tx)
                            :p)))))))

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
  `(new cljs.core/LazySeq nil false (core/fn [] ~@body) nil))

(defmacro delay [& body]
  "Takes a body of expressions and yields a Delay object that will
  invoke the body only the first time it is forced (with force or deref/@), and
  will cache the result and return it on all subsequent force
  calls."
  `(new cljs.core/Delay (atom {:done false, :value nil}) (fn [] ~@body)))

(defmacro binding
  "binding => var-symbol init-expr

  Creates new bindings for the (already-existing) vars, with the
  supplied initial values, executes the exprs in an implicit do, then
  re-establishes the bindings that existed before.  The new bindings
  are made in parallel (unlike let); all init-exprs are evaluated
  before the vars are bound to their new values."
  [bindings & body]
  (core/let [names (take-nth 2 bindings)
        vals (take-nth 2 (drop 1 bindings))
        tempnames (map (comp gensym name) names)
        binds (map vector names vals)
        resets (reverse (map vector names tempnames))]
    (cljs.analyzer/confirm-bindings &env names)
    `(let [~@(interleave tempnames names)]
       (try
        ~@(map
           (core/fn [[k v]] (list 'set! k v))
           binds)
        ~@body
        (finally
         ~@(map
            (core/fn [[k v]] (list 'set! k v))
            resets))))))

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

(defmacro case [e & clauses]
  (core/let [default (if (odd? (count clauses))
                  (last clauses)
                  `(throw (js/Error. (core/str "No matching clause: " ~e))))
        assoc-test (fn assoc-test [m test expr]
                         (if (contains? m test)
                           (throw (clojure.core/IllegalArgumentException.
                                   (core/str "Duplicate case test constant '"
                                             test "'"
                                             (when (:line &env)
                                               (core/str " on line " (:line &env) " "
                                                         cljs.analyzer/*cljs-file*)))))
                           (assoc m test expr)))
        pairs (reduce (fn [m [test expr]]
                        (cond
                         (seq? test) (reduce (fn [m test]
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
        ~@(mapcat (fn [[m c]] `((cljs.core/= ~m ~esym) ~c)) pairs)
        :else ~default))))

(defmacro try
  "(try expr* catch-clause* finally-clause?)

   Special Form

   catch-clause => (catch protoname name expr*)
   finally-clause => (finally expr*)

  Catches and handles JavaScript exceptions."
  [& forms]
  (core/let [catch? #(and (seq? %) (= (first %) 'catch))
        [body catches] (split-with (complement catch?) forms)
        [catches fin] (split-with catch? catches)
        e (gensym "e")]
    (assert (every? #(clojure.core/> (count %) 2) catches) "catch block must specify a prototype and a name")
    (if (seq catches)
      `(~'try*
        ~@body
        (catch ~e
            (cond
             ~@(mapcat
                (core/fn [[_ type name & cb]]
                  `[(instance? ~type ~e) (let [~name ~e] ~@cb)])
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
          (throw (cljs.core/Error.
                  (cljs.core/str "Assert failed: " (cljs.core/pr-str '~x)))))))
  ([x message]
     (when *assert*
       `(when-not ~x
          (throw (cljs.core/Error.
                  (cljs.core/str "Assert failed: " ~message "\n" (cljs.core/pr-str '~x))))))))

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
                                             fs# (seq (iterys# ~next-expr))]
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
                     :else [true `(loop [~seqsym (seq ~v)]
                                    (when ~seqsym
                                      (let [~k (first ~seqsym)]
                                        ~subform
                                        ~@(when needrec [recform]))))]))))]
    (nth (step nil (seq seq-exprs)) 1)))

(defmacro alength [a]
  `(scm* {:a ~a}
         ~'(vector-length :a)))

(defn aclone
  "Returns a javascript array, cloned from the passed in array"
  [a]
  `(scm* {:a ~a} ~'(vector-copy :a)))

(defmacro aclone-push
  "Clones the array into an array one element larger; with the value pushed."
  [a o]
   `(let [sz# (scm* {:a ~a} ~'(vector-length :a))
         r# (scm* {:nsz (inc sz#)} ~'(make-vector :nsz))]
      (scm* {:a ~a :sz sz# :r r# :o ~o}
            ~'(begin
               (subvector-move! :a 0 :sz :r 0)
               (vector-set! :a :sz :o)))))

(defmacro aclone-push2
  "Clones the array into an array one element larger; with the values pushed."
  [a o1 o2]
   `(let [sz# (scm* {:a ~a} ~'(vector-length :a))
         r# (scm* {:nsz (+ sz# 2)} ~'(make-vector :nsz))]
      (scm* {:a ~a :sz sz# :r r# :o1 ~o1 :o2 ~o2}
            ~'(begin
               (subvector-move! :a 0 :sz :r 0)
               (vector-set! :a :sz :o1)
               (vector-set! :a (+ :sz 1) :o2)))))

(defmacro slice
  "does NOT re-create Javascripts negative index sematics, just end >= start slicing. Also, will complain on out-of-ranges; won't be clipped to viable length."
  [a start end]
  `(let [sz# (- ~end ~start)
         r# (scm* {:sz sz#} ~'(make-vector :sz))]
     (scm* {:a ~a :start ~start :end ~end :r r#} ~'(subvector-move! :a :start :end :r 0))))

(defmacro slice-pop
  "returns a copy without the last element"
  [a]
  `(let [sz# (dec (alength ~a))
         r# (scm* {:sz sz#} ~'(make-vector :sz))]
     (scm* {:a ~a :r r#} ~'(subvector-move! :a 0 sz# :r 0))))

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

(defn ^:private check-valid-options
  "Throws an exception if the given option map contains keys not listed
  as valid, else returns nil."
  [options & valid-keys]
  (when (seq (apply disj (apply hash-set (keys options)) valid-keys))
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
               hierarchy# (get ~options :hierarchy cljs.core/global-hierarchy)]
           (cljs.core/MultiFn. ~(name mm-name) ~dispatch-fn ~default hierarchy#
                               method-table# prefer-table# method-cache# cached-hierarchy#))))))

(defmacro defmethod
  "Creates and installs a new method of multimethod associated with dispatch-value. "
  [multifn dispatch-val & fn-tail]
  `(-add-method ~(with-meta multifn {:tag 'cljs.core/MultiFn}) ~dispatch-val (core/fn ~@fn-tail)))

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

(defn gen-apply-to-helper
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
       (let [~'args (seq ~'args)]
         (if (zero? ~'argc)
           (~'f)
           ~(gen-apply-to-helper))))
     (set! ~'*unchecked-if* false)))

(defmacro with-out-str
  "Evaluates exprs in a context in which *print-fn* is bound to .append
  on a fresh StringBuffer.  Returns the string created by any nested
  printing calls."
  [& body]
  `(let [sb# (goog.string/StringBuffer.)]
     (binding [cljs.core/*print-fn* (fn [x#] (.append sb# x#))]
       ~@body)
     (cljs.core/str sb#)))

