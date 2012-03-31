;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.core
  (:refer-clojure :exclude [-> ->> .. amap and areduce alength assert binding bound-fn case comment cond condp
                            declare definline definterface defmethod defmulti defn defn- defonce
                            defprotocol defrecord defstruct deftype delay doseq dosync dotimes doto
                            extend-protocol extend-type fn for future gen-class gen-interface
                            if-let if-not import io! lazy-cat lazy-seq let letfn locking loop
                            memfn ns or proxy proxy-super pvalues refer-clojure reify sync time
                            when when-first when-let when-not while with-bindings with-in-str
                            with-loading-context with-local-vars with-open with-out-str with-precision with-redefs
                            satisfies? identical? true? false?

                            aget aset
                            + - * / < <= > >= == zero? pos? neg? inc dec max min mod
                            bit-and bit-and-not bit-clear bit-flip bit-not bit-or bit-set 
                            bit-test bit-shift-left bit-shift-right bit-xor]))

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
  if-let if-not let letfn loop
  or 
  when when-first when-let when-not while])

(def compare-arglist
  (comparator
   (fn* [arglist-a arglist-b]
     (if (some #{'&} arglist-a)
       false
       (if (some #{'&} arglist-b)
         true
         (core/< (count arglist-a) (count arglist-b)))))))

(defmacro js* [& forms])
(defmacro scm* [& forms]
    `(~'scm* ~@forms))

(defn
  maybe-destructured
  [params body]
  (if (every? symbol? params)
    (cons params body)
    (loop [params params
           new-params []
           lets []]
      (if params
        (if (symbol? (first params))
          (recur (next params) (conj new-params (first params)) lets)
          (let [gparam (gensym "p__")]
            (recur (next params) (conj new-params gparam)
                   (-> lets (conj (first params)) (conj gparam)))))
        `(~new-params
          (let ~lets
            ~@body))))))


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
  (let [name (if (symbol? (first sigs)) (first sigs) nil)
        sigs (if name (next sigs) sigs)
        sigs (if (vector? (first sigs)) (list sigs) sigs)
        psig (fn* [sig]
                  (let [[params & body] sig
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
;        variadic? (core/some #(some #{'&} (first %)) dsigs) don't care if entire fn is.
        combined-sigs
        , (vec (if (core/<= (count dsigs) 1)
                 dsigs
                 (let [[smallest-sig & other-sigs] (sort-by first compare-arglist dsigs)
                       middle-sigs (butlast other-sigs)
                       biggest-sig (last other-sigs)
                       restparam (gensym "rest")
                       bind-rst
                       , (fn* [sig]
                              (let [variadic? (some #{'&} (first sig))
                                    fixed-args (if variadic?
                                                 (butlast (butlast
                                                           (drop (count (first smallest-sig))
                                                                 (first sig))))
                                                 (drop (count (first smallest-sig))
                                                       (first sig)))
                                    rest-arg (when variadic? (last (first sig)))]
                                `(let [~@(mapcat (fn [s m] (when (not= s m) [m s])) ;alias small arg names
                                                 (first smallest-sig) (first sig))
                                       ~@(mapcat (fn [i a] [a `(~'scm* [~restparam] (~'list-ref ~restparam ~i))])
                                                 (range) fixed-args)
                                       ~@(when rest-arg [rest-arg
                                                         `(~'scm* [~restparam] ~(nth (iterate #(list 'cdr %) restparam) (count fixed-args)))])]
                                   ~(second sig))))]
                   `(~(vec (concat (first smallest-sig) ['& restparam])) 
                     (~'case (~'(scm* {} length) ~restparam)
                       0 ~(second smallest-sig)
                       ~@(apply concat
                                (map-indexed
                                 (fn* [i sig] [(clojure.core/inc i) (bind-rst sig)])
                                 middle-sigs))
                       ~(when biggest-sig (bind-rst biggest-sig)))))))]
    (with-meta
      (if name
        (list* 'fn* name combined-sigs)
        (cons 'fn* combined-sigs))
      (meta &form))))

(defmacro identical? [a b]
  `(scm* {:a ~a :b ~b}
         ~'(eqv? :a :b)))

(defmacro true? [x]
  `(identical? ~x true))

(defmacro false? [x]
  `(identical? ~x false))

(defmacro aget [a i]
  `(scm* {:a ~a :i ~i}
         ~'(vector-ref :a :i)))

(defmacro alength [a]
  `(scm* {:a ~a}
         ~'(vector-length :a)))

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
  ([& more] `(scm* ~more ~(cons '< more))))

(defmacro <=
  ([& more] `(scm* ~more ~(cons '<= more))))

(defmacro >
  ([& more] `(scm* ~more ~(cons '> more))))

(defmacro >=
  ([& more] `(scm* ~more ~(cons '>= more))))

(defmacro ==
  ([& more] `(scm* ~more ~(cons '= more))))

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
  `(scm* {:num ~num :div ~div} ~'(modulo :num :div)))

(defmacro bit-not [x]
  `(scm* {:x ~x} ~'(bitwise-not :x)))

(defmacro bit-and
  ([& more] `(scm* ~more ~(cons 'bitwise-and more))))

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

(defn- protocol-prefix [psym]
  (str (.replace (str psym) \. \$) "$"))

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
  (let [t (gensym "t")
        locals (keys (:locals &env))]
   `(do
      (when (undefined? ~t)
        (deftype ~t [~@locals ~'__meta]
          cljs.core.IWithMeta
          (~'-with-meta [~'_ ~'__meta]
            (new ~t ~@locals ~'__meta))
          cljs.core.IMeta
          (~'-meta [~'_] ~'__meta)
          ~@impls))
      (new ~t ~@locals nil))))

#_(defmacro this-as
  "Defines a scope where JavaScript's implicit \"this\" is bound to the name provided."
  [name & body]
  `(let [~name (~'js* "this")]
     ~@body))

(defn gather-polysigs
  "[[x y][x y z]] -> [x y & rst]"
  [sigs]
  (let [smallest-sig (first (sort-by count sigs))]
    (vec (concat smallest-sig
            (if (clojure.core/> (count sigs) 1) ['& 'rst] [])))))

(defn- parse-impls [specs]
  (loop [ret {} s specs]
    (if (seq s)
      (recur (assoc ret (first s) (take-while seq? (next s)))
             (drop-while seq? (next s)))
      ret)))

;It seems this clobbers multi-variadic protocol methods?
(defn- emit-hinted-impl [c [p fs]]
  (let [hint (fn [specs]
               (let [specs (if (vector? (first specs)) 
                             (list specs) 
                             specs)]
                 (map (core/fn [[[target & args] & body]]
                               (cons (apply vector (vary-meta target assoc :tag c) args)
                                     body))
                      specs)))]
    [p
     (apply merge-with (fn [old new] (concat old (drop 2 new)))
            (map (fn [[f & r]] {(-> f name keyword)
                              (cons 'fn (cons f (hint r)))})
                 fs))
     #_(zipmap (map #(-> % first name keyword) fs)
               (map #(cons 'fn (hint (drop 1 %))) fs))]))

(defn- emit-extend-type [c specs]
  (let [impls (parse-impls specs)]
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
  #_(let [resolve #(let [ret (:name (cljs.compiler/resolve-var (dissoc &env :locals) %))]
                   (clojure.core/assert ret (str "Can't resolve: " %))
                   ret)
        impl-map (loop [ret {} s impls]
                   (if (seq s)
                     (recur (assoc ret (first s) (take-while seq? (next s)))
                            (drop-while seq? (next s)))
                     ret))]
    (if (base-type tsym)
      (let [t (base-type tsym)
            assign-impls (core/fn [[p sigs]]
                                  (let [psym (resolve p)
                                        pfn-prefix (subs (str psym) 0 (clojure.core/inc (.lastIndexOf (str psym) ".")))]
                                    (cons `(aset ~psym ~t true)
                                          (map (core/fn [[f & meths]]
                                                        `(aset ~(symbol (str pfn-prefix f)) ~t (fn* ~@meths)))
                                               sigs))))]
        `(do ~@(mapcat assign-impls impl-map)))
      (let [t (resolve tsym)
            prototype-prefix (str t ".prototype.")
            assign-impls,
            (core/fn
             [[p sigs]]
             (let [psym (resolve p)
                   pprefix (protocol-prefix psym)]
               (if (= p 'Object)
                 (let [adapt-params (core/fn [[sig & body]]
                                             (let [[tname & args] sig]
                                               (list (with-meta (vec args) (meta sig))
                                                     (list* 'this-as tname body))))]
                   (map (core/fn [[f & meths]]
                                 `(set! ~(symbol (str prototype-prefix f)) (fn* ~@(map adapt-params meths))))
                        sigs))
                 (map (core/fn [[f & meths]]
                               (let [polyd-meths (if (vector? (first meths)) (list meths) meths) ;normalize (meth [x] ...) to (meth ([x] ...))
                                     _ (print "hey:" f meths polyd-meths)
                                     singlized-args (gather-polysigs (map first meths))]
                                 (list 'scm*
                                       (list 'define-method
                                             (concat [f]
                                                     [(list (first singlized-args) t)]
                                                     (rest singlized-args))
                                             (map rest meths)))))
                      sigs))))]
        `(do ~@(mapcat assign-impls impl-map))))))

(defmacro deftype [t fields & impls]
  (let [adorn-params
        , (core/fn
           [sig]
           (let [meths (if (vector? (second sig)) (list (rest sig)) (rest sig))]
             #_"cons (vary-meta (second sig) assoc :cljs.compiler/fields fields)"
             #_"bind fields to locals: (let [f (Type-f this)]..) could be optized out if unused?"
             (map (fn [[args & body]]
                    (list args `(let ~(vec
                                       (mapcat
                                        (fn [f]
                                          `[~f
                                            , (~(symbol (str t '- f)) ~(first args))]) (remove (into #{} args) fields)))
                                  ~@body)))
                  meths)))
        ;;reshape for extend-type
        dt->et (core/fn [specs]
                        (loop [ret [] s specs]
                          (if (seq s)
                            (recur (-> ret
                                       (conj (first s))
                                       (into
                                        (reduce (core/fn [v [f sigs]]
                                                         (conj v (cons f (first (map adorn-params sigs))))) #_"TODO: Don't know why this is 'first' here"
                                                []
                                                (group-by first (take-while seq? (next s))))))
                                   (drop-while seq? (next s)))
                            ret)))
        r (:name (cljs.compiler/resolve-var (dissoc &env :locals) t))]
    (if (seq impls)
      `(do
         (deftype* ~t ~fields)
         (extend-type ~t ~@(dt->et impls))
         ~t)
      `(do
         (deftype* ~t ~fields)
         ~t))))

(defn- emit-defrecord
   "Do not use this directly - use defrecord"
  [tagname rname fields impls]
  (let [hinted-fields fields
        fields (vec (map #(with-meta % nil) fields))
        base-fields fields
	fields (conj fields '__meta '__extmap)
	adorn-params (core/fn [sig]
                       (cons (vary-meta (second sig) assoc :cljs.compiler/fields fields)
                             (nnext sig)))
        ;;reshape for extend-type
        dt->et (core/fn [specs]
                 (loop [ret [] s specs]
                   (if (seq s)
                     (recur (-> ret
                                (conj (first s))
                                (into
                                 (reduce (core/fn [v [f sigs]]
                                           (conj v (cons f (map adorn-params sigs))))
                                         []
                                         (group-by first (take-while seq? (next s))))))
                            (drop-while seq? (next s)))
                     ret)))]
    (let [gs (gensym)
	  impls (concat
		 impls
		 ['IRecord
		  'IHash
		  `(~'-hash [this#] (hash-coll this#))
		  'IEquiv
		  `(~'-equiv [this# other#]
         (and (identical? (.-constructor this#)
                          (.-constructor other#))
              (equiv-map this# other#)))
		  'IMeta
		  `(~'-meta [this#] ~'__meta)
		  'IWithMeta
		  `(~'-with-meta [this# ~gs] (new ~tagname ~@(replace {'__meta gs} fields)))
		  'ILookup
		  `(~'-lookup [this# k#] (-lookup this# k# nil))
		  `(~'-lookup [this# k# else#]
			      (get (merge (hash-map ~@(mapcat (core/fn [fld] [(keyword fld) fld]) 
							      base-fields))
					  ~'__extmap)
				   k# else#))
		  'ICounted
		  `(~'-count [this#] (+ ~(count base-fields) (count ~'__extmap)))
		  'ICollection
		  `(~'-conj [this# entry#]
      		       (if (vector? entry#)
      			 (-assoc this# (-nth entry# 0) (-nth entry# 1))
      			 (reduce -conj
      				 this#
      				 entry#)))
		  'IAssociative
		  `(~'-assoc [this# k# ~gs]
                     (condp identical? k#
                       ~@(mapcat (core/fn [fld]
                                   [(keyword fld) (list* `new tagname (replace {fld gs} fields))])
                                 base-fields)
                       (new ~tagname ~@(remove #{'__extmap} fields) (assoc ~'__extmap k# ~gs))))
		  'IMap
		  `(~'-dissoc [this# k#] (if (contains? #{~@(map keyword base-fields)} k#)
                                            (dissoc (with-meta (into {} this#) ~'__meta) k#)
                                            (new ~tagname ~@(remove #{'__extmap} fields) 
                                                 (not-empty (dissoc ~'__extmap k#)))))
		  'ISeqable
		  `(~'-seq [this#] (seq (concat [~@(map #(list `vector (keyword %) %) base-fields)] 
                                              ~'__extmap)))
		  'IPrintable
		  `(~'-pr-seq [this# opts#]
			      (let [pr-pair# (core/fn [keyval#] (pr-sequential pr-seq "" " " "" opts# keyval#))]
				(pr-sequential
				 pr-pair# (str "#" ~(name rname) "{") ", " "}" opts#
				 (concat [~@(map #(list `vector (keyword %) %) base-fields)] 
					 ~'__extmap))))
		  ])]
      `(do
	 (~'deftype* ~tagname ~hinted-fields)
	 (extend-type ~tagname ~@(dt->et impls))))))

(defn- build-positional-factory
  [rsym rname fields]
  (let [fn-name (symbol (str '-> rsym))]
    `(defn ~fn-name
       [~@fields]
       (new ~rname ~@fields))))

(defn- build-map-factory
  [rsym rname fields]
  (let [fn-name (symbol (str 'map-> rsym))
	ms (gensym)
	ks (map keyword fields)
	getters (map (core/fn [k] `(~k ~ms)) ks)]
    `(defn ~fn-name
       [~ms]
       (new ~rname ~@getters nil (dissoc ~ms ~@ks)))))

(defmacro defrecord [rsym fields & impls]
  (let [r (:name (cljs.compiler/resolve-var (dissoc &env :locals) rsym))]
    `(let []
       ~(emit-defrecord rsym r fields impls)
       (set! (.-cljs$core$IPrintable$_pr_seq ~r) (core/fn [this#] (list ~(str r))))
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
  (let [p (:name (cljs.compiler/resolve-var (dissoc &env :locals) psym))
        ns-name (-> &env :ns :name)
        fqn (core/fn [n] (symbol (str ns-name "." n)))
        prefix (protocol-prefix p)
        methods (if (string? (first doc+methods)) (next doc+methods) doc+methods)
        method
        , (core/fn
           [[fname & sigs]]
           (let [single-sig (gather-polysigs (take-while vector? sigs))
                 [o & rst] single-sig
                 _ (when (= o '&) (throw "Can't have polyvariadic protocol methods with no fixed args."))
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
                                                 (map #(get {'& '.} % %) rst)))
                             ::prim-dispatches)))))]
    `(do
       (def ~psym (quote ~p))
       ~@(map method methods))))

(defmacro satisfies?
  "Returns true if x satisfies the protocol"
  [psym x]
  (let [p (:name (cljs.compiler/resolve-var (dissoc &env :locals) psym))
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
  `(new cljs.core/LazySeq nil false (core/fn [] ~@body)))

(defmacro binding
  "binding => var-symbol init-expr

  Creates new bindings for the (already-existing) vars, with the
  supplied initial values, executes the exprs in an implicit do, then
  re-establishes the bindings that existed before.  The new bindings
  are made in parallel (unlike let); all init-exprs are evaluated
  before the vars are bound to their new values."
  [bindings & body]
  (let [names (take-nth 2 bindings)
        vals (take-nth 2 (drop 1 bindings))
        tempnames (map (comp gensym name) names)
        binds (map vector names vals)
        resets (reverse (map vector names tempnames))]
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
  (let [gpred (gensym "pred__")
        gexpr (gensym "expr__")
        emit (core/fn emit [pred expr args]
               (let [[[a b c :as clause] more]
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

(defmacro try
  "(try expr* catch-clause* finally-clause?)

   Special Form

   catch-clause => (catch protoname name expr*)
   finally-clause => (finally expr*)

  Catches and handles JavaScript exceptions."
  [& forms]
  (let [catch? #(and (list? %) (= (first %) 'catch))
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

(defmacro ^{:private true} assert-args [fnname & pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                  ~(str fnname " requires " (second pairs)))))
     ~(let [more (nnext pairs)]
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

  (take 100 (for [x (range 100000000) y (range 1000000) :while (< y x)]  [x y]))"
  [seq-exprs body-expr]
  (assert-args for
     (vector? seq-exprs) "a vector for its binding"
     (even? (count seq-exprs)) "an even number of forms in binding vector")
  (let [to-groups (core/fn [seq-exprs]
                    (reduce (core/fn [groups [k v]]
                              (if (keyword? k)
                                (conj (pop groups) (conj (peek groups) [k v]))
                                (conj groups [k v])))
                            [] (partition 2 seq-exprs)))
        err (core/fn [& msg] (throw (apply str msg)))
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
  (let [step (core/fn step [recform exprs]
               (if-not exprs
                 [true `(do ~@body)]
                 (let [k (first exprs)
                       v (second exprs)
                       
                       seqsym (when-not (keyword? k) (gensym))
                       recform (if (keyword? k) recform `(recur (first ~seqsym) ~seqsym))
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
                     :else [true `(let [~seqsym (seq ~v)]
                                    (when ~seqsym
                                      (loop [~k (first ~seqsym) ~seqsym ~seqsym]
                                       ~subform
                                       (when-let [~seqsym (next ~seqsym)]
                                        ~@(when needrec [recform])))))]))))]
    (nth (step nil (seq seq-exprs)) 1)))

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
  (let [i (first bindings)
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
     (apply str "Only these options are valid: "
	    (first valid-keys)
	    (map #(str ", " %) (rest valid-keys))))))

(defmacro defmulti
  "Creates a new multimethod with the associated dispatch function.
  The docstring and attribute-map are optional.

  Options are key-value pairs and may be one of:
    :default    the default dispatch value, defaults to :default
    :hierarchy  the isa? hierarchy to use for dispatching
                defaults to the global hierarchy"
  [mm-name & options]
  (let [docstring   (if (string? (first options))
                      (first options)
                      nil)
        options     (if (string? (first options))
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
    (let [options   (apply hash-map options)
          default   (get options :default :default)
          ;; hierarchy (get options :hierarchy #'cljs.core.global-hierarchy)
	  ]
      (check-valid-options options :default :hierarchy)
      `(def ~(with-meta mm-name m)
	 (let [method-table# (atom {})
	       prefer-table# (atom {})
	       method-cache# (atom {})
	       cached-hierarchy# (atom {})
	       hierarchy# (get ~options :hierarchy cljs.core/global-hierarchy)
	       ]
	   (cljs.core.MultiFn. ~(name mm-name) ~dispatch-fn ~default hierarchy#
			       method-table# prefer-table# method-cache# cached-hierarchy#))))))

(defmacro defmethod
  "Creates and installs a new method of multimethod associated with dispatch-value. "
  [multifn dispatch-val & fn-tail]
  `(-add-method ~(with-meta multifn {:tag 'cljs.core.MultiFn}) ~dispatch-val (core/fn ~@fn-tail)))

(defmacro time
  "Evaluates expr and prints the time it took. Returns the value of expr."
  [expr]
  `(let [start# (.getTime (js/Date.) ())
         ret# ~expr]
     (prn (str "Elapsed time: " (- (.getTime (js/Date.) ()) start#) " msecs"))
     ret#))
