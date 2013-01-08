;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(set! *warn-on-reflection* true)

(ns cljs.compiler
  (:refer-clojure :exclude [munge macroexpand-1])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.walk :as walk]
;            [cljs.tagged-literals :as tags] 
            [cljs.analyzer :as ana])
  (:import java.lang.StringBuilder))

                                        ;Game plan : use emits, not print.
                                        ;unwrap nested emits.
                                        ; mapped emits - emits already nests into sequences.
                                        ; Q: For string things like space sep?? Looks like the way to do it is guard the emits as thunks that are sep'd by commas? Really is that the best way (only done when flipping to constant-emitting - otherwise we just leave the seq of maps be.) Or guard with-out-str if you're doing some string ops on the result.
                                        ; note that print spaces args, emits doesn't.
                                        ;don't use emit-wrap (no expression/statement dichotomy.)
(def js-reserved
  #{"abstract" "boolean" "break" "byte" "case"
    "catch" "char" "class" "const" "continue"
    "debugger" "default" "delete" "do" "double"
    "else" "enum" "export" "extends" "final"
    "finally" "float" "for" "function" "goto" "if"
    "implements" "import" "in" "instanceof" "int"
    "interface" "let" "long" "native" "new"
    "package" "private" "protected" "public"
    "return" "short" "static" "super" "switch"
    "synchronized" "this" "throw" "throws"
    "transient" "try" "typeof" "var" "void"
    "volatile" "while" "with" "yield" "methods"})

(def ^:dynamic *position* nil)
(def ^:dynamic *emitted-provides* nil)
(def ^:dynamic *lexical-renames* {})
(def cljs-reserved-file-names #{"deps.cljs"})

(defn munge ([s] s) ([s r] s))

(defn dispatch-munge [s]
  (-> s
      (clojure.string/replace "." "_")
      (clojure.string/replace "/" "$")))

(defn strict-str
  "recursively realizes any potentially lazy nested structure"
  ([] "")
  ([x] (str (walk/postwalk (fn [n] (cond (= clojure.lang.LazySeq (type n)) (apply list n)
                                         (identical? x ()) "()" ;o/w clojure.lang.PersistentList$EmptyList@1
                                         :else n)) x)))
  ([x & ys] (str (strict-str x) (apply strict-str ys))))

(defonce ns-first-segments (atom '#{"cljs" "clojure"}))

#_(defn munge
  ([s] (munge s js-reserved))
  ([s reserved]
    (if (map? s)
      ; Unshadowing
      (let [{:keys [name field] :as info} s
            depth (loop [d 0, {:keys [shadow]} info]
                    (cond
                      shadow (recur (inc d) shadow)
                      (@ns-first-segments (str name)) (inc d)
                      :else d))
            renamed (*lexical-renames* (System/identityHashCode s))
            munged-name (munge (cond field (str "self__." name)
                                     renamed renamed
                                     :else name)
                               reserved)]
        (if (or field (zero? depth))
          munged-name
          (symbol (str munged-name "__$" depth))))
      ; String munging
      (let [ss (string/replace (str s) #"\/(.)" ".$1") ; Division is special
            ss (apply str (map #(if (reserved %) (str % "$") %)
                               (string/split ss #"(?<=\.)|(?=\.)")))
            ms (clojure.lang.Compiler/munge ss)]
        (if (symbol? s)
          (symbol ms)
          ms)))))

(defn- comma-sep [xs]
  (interpose "," xs))
(defn- space-sep [xs]
  (interpose " " xs))

(defn- escape-char [^Character c]
  (let [cp (.hashCode c)]
    (case cp
      ; Handle printable escapes before ASCII
      34 "\\\""
      92 "\\\\"
      ; Handle non-printable escapes
      8 "\\b"
      12 "\\f"
      10 "\\n"
      13 "\\r"
      9 "\\t"
      (if (< 31 cp 127)
        c ; Print simple ASCII characters
        (format "\\u%04X" cp)))))

(defn- escape-string [^CharSequence s]
  (let [sb (StringBuilder. (count s))]
    (doseq [c s]
      (.append sb (escape-char c)))
    (.toString sb)))

(defn- wrap-in-double-quotes [x]
  (str \" x \"))

(defmulti emit :op)

(defn emits [& xs]
  (doseq [x xs]
    (cond
      (nil? x) nil
      (map? x) (emit x)
      (coll? x) (apply emits x)
      (fn? x)  (x)
      :else (do
              (let [s (print-str x)]
                (when *position*
                  (swap! *position* (fn [[line column]]
                                      [line (+ column (count s))])))
                (print s)))))
  nil)

(defn- print-scm [fun-str & children]
  (print (str "(" fun-str " " (space-sep (map emits children)) ")")))

(defn ^String emit-str [expr]
  (with-out-str (emit expr)))

(defn emitln [& xs]
  (apply emits xs)
  ;; Prints column-aligned line number comments; good test of *position*.
  ;(when *position*
  ;  (let [[line column] @*position*]
  ;    (print (apply str (concat (repeat (- 120 column) \space) ["// " (inc line)])))))
  (println)
  (when *position*
    (swap! *position* (fn [[line column]]
                        [(inc line) 0])))
  nil)

(defn ^String emit-str [expr]
  (with-out-str (emit expr)))

(defn emit-provide [sym]
  (when-not (or (nil? *emitted-provides*) (contains? @*emitted-provides* sym))
    (swap! *emitted-provides* conj sym)
    (emitln "goog.provide('" (munge sym) "');")))

(defmulti emit-constant class)
(defmethod emit-constant nil [x] (emits "#!void"))
(defmethod emit-constant Long [x] (emits x))
(defmethod emit-constant Integer [x] (emits x)) ; reader puts Integers in metadata
(defmethod emit-constant Double [x] (emits x))
(defmethod emit-constant String [x]
  (emits (wrap-in-double-quotes (escape-string x))))
(defmethod emit-constant Boolean [x] (emits (if x "#t" "#f")))
(defmethod emit-constant Character [x] (emits "#\\" x))

(defmethod emit-constant java.util.regex.Pattern [x]
  (let [[_ flags pattern] (re-find #"^(?:\(\?([idmsux]*)\))?(.*)" (str x))]
    (emits \/ (.replaceAll (re-matcher #"/" pattern) "\\\\/") \/ flags)))

(defmethod emit-constant clojure.lang.Keyword [x]
  (emits (if (namespace x)
                (str (namespace x) "/") "")
              (name x)
              ":"))

(def ^:dynamic *quoted* false)
(defmethod emit-constant clojure.lang.Symbol [x]
           (emits  (when-not *quoted* \')
                    (if (namespace x)
                      (str (namespace x) "/") "")
                    (name x)))

(defn- emit-meta-constant [x & body]
  (if (meta x)
    (do
      (emits "cljs.core.with_meta(" body ",") ;TODO new cljs
      (emit-constant (meta x))
      (emits ")"))
    (emits body)))

(defmethod emit-constant clojure.lang.PersistentList$EmptyList [x]
  (print (when (not *quoted*) "'") "()"))

(defmethod emit-constant clojure.lang.PersistentList [x]
  (emit-meta-constant x
    (when (not *quoted*) "'") "("
              (binding [*quoted* true]
                (space-sep (map #(with-out-str (emit-constant %)) x)))
              ")"))

(defmethod emit-constant clojure.lang.Cons [x]
  (emit-meta-constant x
    (when (not *quoted*) "'") "("
              (binding [*quoted* true]
                (space-sep (map #(with-out-str (emit-constant %)) x)))
              ")"))

(declare analyze)

(defmethod emit-constant clojure.lang.IPersistentVector [x]
  (emit-meta-constant x
   "'#("
        (binding [*quoted* true]
          (space-sep (map #(with-out-str (emit-constant %)) x)))
        ")"))

(defmethod emit-constant clojure.lang.IPersistentMap [x]
  (emit-meta-constant x
    "(cljs.core/hash-map "
    (space-sep (map #(with-out-str (emit-constant %))
                    (apply concat x)))
    ")"))

(defmethod emit-constant clojure.lang.PersistentHashSet [x]
  (emit-meta-constant x
    "(cljs.core/set (list "
    (space-sep (map #(with-out-str (emit-constant %)) x))
    "))"))

(defn emit-block
  [context statements ret]
  (if statements
    (emits (apply strict-str
                    (concat ["(begin "]
                            (interpose "\n  " (map #(with-out-str (emit %))
                                                 (concat statements [ret])))
                            [")"])))
    (emit ret)))

(defmacro emit-wrap [env & body]
  `(let [env# ~env]
     (when (= :return (:context env#)) (emits "return "))
     ~@body
     (when-not (= :expr (:context env#)) (emitln ";"))))

(defmethod emit :no-op [m])

(defmethod emit :var
  [{:keys [info env] :as arg}]
  (let [n (:name info)
        n (if (= (namespace n) "js")
            (name n)
            info)]
    (when-not (= :statement (:context env))
      (emit-wrap env (emits (munge n))))))

(defmethod emit :meta
  [{:keys [expr meta env]}]
  (emits "(cljs.core/with-meta " expr " " emits meta ")"))

(def ^:private array-map-threshold 16)
(def ^:private obj-map-threshold 32)

(defmethod emit :map
  [{:keys [env simple-keys? keys vals]}]
  (let [sz (count keys)
        table-name (gensym "table")]
    (emits "(let (("table-name" (make-table size: "sz")))")
    (emits (mapcat (fn [k v] ["(table-set!" table-name k v")"]) keys vals))
    (emits table-name ")")))

(defmethod emit :vector
  [{:keys [items env]}]
  (emits "(vector " (space-sep items)")"))

(defmethod emit :set
  [{:keys [items env]}]
  (emits "(cljs.core/set (list "
         (space-sep items)"))"))

(defmethod emit :constant
  [{:keys [form env]}]
  (emit-constant form))

(defn get-tag [e]
  (or (-> e :tag)
      (-> e :info :tag)))

(defn infer-tag [e]
  (if-let [tag (get-tag e)]
    tag
    (case (:op e)
      :let (infer-tag (:ret e))
      :if (let [then-tag (infer-tag (:then e))
                else-tag (infer-tag (:else e))]
            (when (= then-tag else-tag)
              then-tag))
      :constant (case (:form e)
                  true 'boolean
                  false 'boolean
                  nil)
      nil)))

(defn safe-test? [e]
  (let [tag (infer-tag e)]
    (or (#{'boolean 'seq} tag)
        (when (= (:op e) :constant)
          (let [form (:form e)]
            (not (or (and (string? form) (= form ""))
                     (and (number? form) (zero? form)))))))))

(defmethod emit :if
  [{:keys [test then else env unchecked]}]
  (let [checked (not (or unchecked (safe-test? test)))]
    (if checked
      (let [t (gensym "test")]
        (emits "(let ((" t " "  test ")) (if (and "t" (not (eq? #!void "t"))) "
             then " "
             else"))"))
      (emits (space-sep "(if" test then else")")))))

(defmethod emit :case
  [{:keys [test clauses else]}]
  (emits "(case " test " ")
  (emits (for [[test result] clauses]
           ["(("(space-sep test)")"
            " " result ")"]))
  (when else
    (emits "(else " else ")"))
  (emits ")"))

(defmethod emit :throw
  [{:keys [throw env]}]
  (emits "(raise " throw ")"))

(defn emit-comment
  "Emit a nicely formatted comment string."
  [doc jsdoc]
  (let [docs (when doc [doc])
        docs (if jsdoc (concat docs jsdoc) docs)
        docs (remove nil? docs)]
    (letfn [(print-comment-lines [e] (doseq [next-line (string/split-lines e)]
                                       (emitln ";" (string/trim next-line))))]
      (when (seq docs)
        (doseq [e docs]
          (when e
            (print-comment-lines e)))))))

(defmethod emit :def
  [{:keys [name init env doc export]}]
  (if init
    (do
      (emit-comment doc (:jsdoc init))
      (emits "(define " name init)
      (emits "\n"))
    (emits "(define " name")\n")
    #_(when export
        (println (str "goog.exportSymbol('" export "', " name ");")))))
(defn schemify-method-arglist
  "analyzed method [a b & r] -> (a b . r) -- as symbols not as a string.
   or [& r] -> r in the case of no fixed args."
  [{:keys [variadic max-fixed-arity params]}]
  (if variadic
    (if (> max-fixed-arity 0)
      (apply list (concat (take max-fixed-arity params)
                          ['. (last  params)]))
      (last params))
    (apply list params)))

(defn emit-fn-method
  [{:keys [gthis name variadic params statements ret env recurs max-fixed-arity] :as f}]
  (let [lambda-str (strict-str "(lambda "
                               (schemify-method-arglist f) " "
                               (when variadic (str "(let (("(last params) " (cljs.core/-seq " (last params) "))) "))
                               (with-out-str (emit-block :return statements ret))
                               (when variadic ")")
                               ")")]
    (if name
      (emits "(letrec (("name" "lambda-str")) "name")")
      (emits lambda-str))))

(defmethod emit :fn
  [{:keys [name env methods max-fixed-arity variadic recur-frames]}]
  (when-not (= :statement (:context env))
    (let [loop-locals (->> (concat (mapcat :params (filter #(and % @(:flag %)) recur-frames))
                                   (mapcat :params loop-lets))
                           (map munge)
                           seq)
          recur-name (:recur-name env)]
      (if (= 1 (count methods))
        (emit-fn-method (assoc (first methods) :name (or name recur-name)))
        (throw (Exception. "Expected multiarity to be erased in macros."))))))

(defmethod emit :extend
  [{:keys [etype impls base-type?]}]
  (doall
   (for [[protocol meth-map] impls]
     (do
       (emit-comment (str "Implementing " (:name protocol)) nil)
       (println "(table-set! "
                "(table-ref cljs.core/protocol-impls" (:name etype) ")"
                (:name protocol) "#t" ")")
       (doall
        (for [[meth-name meth-impl] meth-map]
          (let [meth (first (:methods meth-impl))
                fn-scm-args (schemify-method-arglist meth) ;FIXME may not be a seq.
                rest? (some #{'.} fn-scm-args)
                fun-str (emits meth-impl)
                impl-name (symbol (str (:name (:info meth-name))
                                       "---" (dispatch-munge (:name etype))))]
            (when (> (count (:methods meth-impl)) 1) (throw (Exception. "should have compiled variadic defn away.")))
            (println
             (str "(define " (cons impl-name 
                                   fn-scm-args)) 
             (if rest?
               (str "(apply " fun-str " (append (list "
                    (space-sep (butlast (:params meth))) ") "
                    (last (:params meth)) "))")
               (str "(" fun-str " " (space-sep (:params meth)) ")"))
             ")")
            (when-not base-type?
              (println (str "(table-set! " (:name (:info meth-name)) "---vtable")
                       (:name etype)
                       impl-name ")") ))))))))

(defmethod emit :do
  [{:keys [statements ret env]}]
  (let [context (:context env)]
    (emit-block context statements ret)))

(defmethod emit :try*
  [{:keys [env try catch name finally]}]
  (let [context (:context env)
        subcontext (if (= :expr context) :return context)]
    (if (or name finally)
      (do
        (when finally (throw (Exception. (str "finally not yet implemented")))) ;TODO
        (emits "(with-exception-catcher ")
        (when name
          (emits "(lambda (" name ") ")
          (when catch
            (let [{:keys [statements ret]} catch]
              (emit-block subcontext statements ret)))
          (emits ")"))
        (emits "(lambda () ")
        (let [{:keys [statements ret]} try]
          (emit-block subcontext statements ret))
        (emits "))"))
      (let [{:keys [statements ret]} try]
        (emit-block subcontext statements ret)))))

(defmethod emit :let
  [{:keys [bindings statements ret env loop recur-name]}]
  (let [bs (map (fn [{:keys [name init]}]
                  (str "(" name " " (emits init) ")"))
                bindings)
        context (:context env)]
    (binding [*lexical-renames* (into *lexical-renames*
                                      (when (= :statement context)
                                        (map #(vector (System/identityHashCode %)
                                                      (gensym (str (:name %) "-")))
                                             bindings)))]
      (emits "(let* (" bs ")")
      (when loop (emits "(letrec ((" recur-name
                        "(lambda (" (space-sep (map :name bindings)) ")" ))
      (emit-block (if (= :expr context) :return context) statements ret)
      (when loop (emits ")))" "(" recur-name " " (space-sep (map :name bindings)) "))"))
      (emits ")"))))

(defmethod emit :recur
  [{:keys [frame exprs env]}]
  (let [temps (vec (take (count exprs) (repeatedly gensym)))
        params (:params frame)
        recur-name (:recur-name env)]
    (emits "(" recur-name " " (space-sep (map emits exprs))")")))

(defmethod emit :letfn
  [{:keys [bindings statements ret env]}] :TODO)
#_(defmethod emit :letfn
  [{:keys [bindings statements ret env]}]
  (let [context (:context env)]
    (when (= :expr context) (emits "(function (){"))
    (doseq [{:keys [init] :as binding} bindings]
      (emitln "var " (munge binding) " = " init ";"))
    (emit-block (if (= :expr context) :return context) statements ret)
    (when (= :expr context) (emits "})()"))))

(defn protocol-prefix [psym]
  (symbol (str (-> (str psym) (.replace \. \$) (.replace \/ \$)) "$")))

(defmethod emit :invoke ; TODO -- this is ignoring all new protocol stuff.
  [{:keys [f args env] :as expr}]
  (let [info (:info f)
        fn? (and ana/*cljs-static-fns*
                 (not (:dynamic info))
                 (:fn-var info))
        protocol (:protocol info)
        proto? (let [tag (infer-tag (first (:args expr)))]
                 (and protocol tag
                      (or ana/*cljs-static-fns*
                          (:protocol-inline env))
                      (or (= protocol tag)
                          (when-let [ps (:protocols (ana/resolve-existing-var (dissoc env :locals) tag))]
                            (ps protocol)))))
        opt-not? (and (= (:name info) 'cljs.core/not)
                      (= (infer-tag (first (:args expr))) 'boolean))
        ns (:ns info)
        js? (= ns 'js)
        goog? (when ns
                (or (= ns 'goog)
                    (when-let [ns-str (str ns)]
                      (= (get (string/split ns-str #"\.") 0 nil) "goog"))))
        keyword? (and (= (-> f :op) :constant)
                      (keyword? (-> f :form)))
        [f variadic-invoke]
        (if fn?
          (let [arity (count args)
                variadic? (:variadic info)
                mps (:method-params info)
                mfa (:max-fixed-arity info)]
            (cond
             ;; if only one method, no renaming needed
             (and (not variadic?)
                  (= (count mps) 1))
             [f nil]

             ;; direct dispatch to variadic case
             (and variadic? (> arity mfa))
             [(update-in f [:info :name]
                             (fn [name] (symbol (str (munge name) ".cljs$lang$arity$variadic"))))
              {:max-fixed-arity mfa}]

             ;; direct dispatch to specific arity case
             :else
             (let [arities (map count mps)]
               (if (some #{arity} arities)
                 [(update-in f [:info :name]
                             (fn [name] (symbol (str (munge name) ".cljs$lang$arity$" arity)))) nil]
                 [f nil]))))
          [f nil])]
    ;;TODO leverage static information
    #_(emit-wrap env
      (cond
       opt-not?
       (emits "!(" (first args) ")")

       proto?
       (let [pimpl (str (munge (protocol-prefix protocol))
                        (munge (name (:name info))) "$arity$" (count args))]
         (emits (first args) "." pimpl "(" (comma-sep args) ")"))

       keyword?
       (emits "(new cljs.core.Keyword(" f ")).call(" (comma-sep (cons "null" args)) ")")
       
       variadic-invoke
       (let [mfa (:max-fixed-arity variadic-invoke)]
        (emits f "(" (comma-sep (take mfa args))
               (when-not (zero? mfa) ",")
               "cljs.core.array_seq([" (comma-sep (drop mfa args)) "], 0))"))
       
       (or fn? js? goog?)
       (emits f "(" (comma-sep args)  ")")
       
       :else
       (if (and ana/*cljs-static-fns* (= (:op f) :var))
         (let [fprop (str ".cljs$lang$arity$" (count args))]
           (emits "(" f fprop " ? " f fprop "(" (comma-sep args) ") : " f ".call(" (comma-sep (cons "null" args)) "))"))
         (emits f ".call(" (comma-sep (cons "null" args)) ")"))))
    (emits "("f" "(space-sep args)")")))

(defmethod emit :new
  [{:keys [ctor args env]}]
  (emits "(make-" ctor " "
              (space-sep args)
              ")"))

(defmethod emit :set!
  [{:keys [target val env]}]
  (if (= :dot (:op target))
    (emits "(cljs.core/record-set! " (:target target) " '" (:field target) " " val")")
    (emits "(set! " target " " val ")")))

(defmethod emit :ns
  [{:keys [name requires uses requires-macros env]}]
  (swap! ns-first-segments conj (first (string/split (str name) #"\.")))
  (when-not (= name 'cljs.core)
    (emits "(load \"cljs.core\")"))
  (doseq [lib (into (vals requires) (distinct (vals uses)))]
    (emits "(load \"" (munge lib) "\")")))

(defmethod emit :deftype*
  [{:keys [t fields no-constructor]}]
  (let [fields (map munge fields)]
    #_(emit-provide t)
    (emitln "(define-type " t " " (space-sep fields) (if no-constructor " constructor: #f" "") ")")
    (emitln "(define " t " ##type-" (count fields)  "-" t ")") 
    (emitln "(table-set! cljs.core/protocol-impls " t " (make-table))" )))


(comment (defmethod emit :defrecord*
  [{:keys [t fields pmasks]}]
  (let [fields (concat (map munge fields) '[__meta __extmap])]
    (emit-provide t)
    (emitln "")
    (emitln "/**")
    (emitln "* @constructor")
    (doseq [fld fields]
      (emitln "* @param {*} " fld))
    (emitln "* @param {*=} __meta ")
    (emitln "* @param {*=} __extmap")
    (emitln "*/")
    (emitln (munge t) " = (function (" (comma-sep fields) "){")
    (doseq [fld fields]
      (emitln "this." fld " = " fld ";"))
    (doseq [[pno pmask] pmasks]
      (emitln "this.cljs$lang$protocol_mask$partition" pno "$ = " pmask ";"))
    (emitln "if(arguments.length>" (- (count fields) 2) "){")
    (emitln "this.__meta = __meta;")
    (emitln "this.__extmap = __extmap;")
    (emitln "} else {")
    (emits "this.__meta=")
    (emit-constant nil)
    (emitln ";")
    (emits "this.__extmap=")
    (emit-constant nil)
    (emitln ";")
    (emitln "}")
    (emitln "})"))))

(defmethod emit :dot
  [{:keys [target field method args env]}]
  (if field
    (emit "(cljs.core/record-ref " target " '"  field ")")
    (throw (Exception. (str "no special dot-method access: " (:line env)))) ;TODO
    #_(emits target "." (munge method #{}) "("
                      (comma-sep args)
                      ")")))

(defmethod emit :scm-str
  [{:keys [env code segs args]}]
  (emit-wrap env
             (if code
               (emits code)
               (emits (interleave (concat segs (repeat nil))
                                  (concat args [nil]))))))

;form->form mapping (or a vector of candidate forms) that will be subject to analyze->emit in context.
(defmethod emit :scm
  [{:keys [env symbol-map form]}]
  (let [symbol-map (if (and (coll? symbol-map) (not (map? symbol-map)))
                     (into {} (map vector symbol-map symbol-map))
                     symbol-map)
        subbed-form (walk/prewalk
                     (fn [t] (let [r (get symbol-map t ::not-found)]
                               (if (= ::not-found r)
                                 t
                                 (symbol (emits (analyze (assoc env :context :return) r)))))) form)]
    (print (space-sep subbed-form))))

(defn forms-seq
  "Seq of forms in a Clojure or ClojureScript file."
  ([f]
     (forms-seq f (clojure.lang.LineNumberingPushbackReader. (io/reader f))))
  ([f ^java.io.PushbackReader rdr]
     (if-let [form (binding [*ns* ana/*reader-ns*] (read rdr nil nil))]
       (lazy-seq (cons form (forms-seq f rdr)))
       (.close rdr))))

(defn rename-to-scm
  "Change the file extension from .cljs to .js. Takes a File or a
  String. Always returns a String."
  [file-str]
  (clojure.string/replace file-str #".cljs$" ".scm"))

(defn mkdirs
  "Create all parent directories for the passed file."
  [^java.io.File f]
  (.mkdirs (.getParentFile (.getCanonicalFile f))))

(defmacro with-core-cljs
  "Ensure that core.cljs has been loaded."
  [& body]
  `(do (when-not (:defs (get @ana/namespaces 'cljs.core))
         (ana/analyze-file "cljs/core.cljs"))
       ~@body))

(defn compile-file* [src dest]
  (with-core-cljs
    (with-open [out ^java.io.Writer (io/make-writer dest {})]
      (binding [*out* out
                ana/*cljs-ns* 'cljs.user
                ana/*cljs-file* (.getPath ^java.io.File src)
                *data-readers* tags/*cljs-data-readers*
                *position* (atom [0 0])
                *emitted-provides* (atom #{})]
        (loop [forms (forms-seq src)
               ns-name nil
               deps nil]
          (if (seq forms)
            (let [env (ana/empty-env)
                  ast (ana/analyze env (first forms))]
              (do (emit ast)
                  (if (= (:op ast) :ns)
                    (recur (rest forms) (:name ast) (merge (:uses ast) (:requires ast)))
                    (recur (rest forms) ns-name deps))))
            {:ns (or ns-name 'cljs.user)
             :provides [ns-name]
             :requires (if (= ns-name 'cljs.core) (set (vals deps)) (conj (set (vals deps)) 'cljs.core))
             :file dest}))))))

(defn requires-compilation?
  "Return true if the src file requires compilation."
  [^java.io.File src ^java.io.File dest]
  (or (not (.exists dest))
      (> (.lastModified src) (.lastModified dest))))

(defn parse-ns [src dest]
  (with-core-cljs
    (binding [ana/*cljs-ns* 'cljs.user]
      (loop [forms (forms-seq src)]
        (if (seq forms)
          (let [env (ana/empty-env)
                ast (ana/analyze env (first forms))]
            (if (= (:op ast) :ns)
              (let [ns-name (:name ast)
                    deps    (merge (:uses ast) (:requires ast))]
                {:ns (or ns-name 'cljs.user)
                 :provides [ns-name]
                 :requires (if (= ns-name 'cljs.core)
                             (set (vals deps))
                             (conj (set (vals deps)) 'cljs.core))
                 :file dest})
              (recur (rest forms)))))))))

(defn compile-file
  "Compiles src to a file of the same name, but with a .js extension,
   in the src file's directory.

   With dest argument, write file to provided location. If the dest
   argument is a file outside the source tree, missing parent
   directories will be created. The src file will only be compiled if
   the dest file has an older modification time.

   Both src and dest may be either a String or a File.

   Returns a map containing {:ns .. :provides .. :requires .. :file ..}.
   If the file was not compiled returns only {:file ...}"
  ([src]
     (let [dest (rename-to-scm src)]
       (compile-file src dest)))
  ([src dest]
     (let [src-file (io/file src)
           dest-file (io/file dest)]
       (if (.exists src-file)
         (if (requires-compilation? src-file dest-file)
           (do (mkdirs dest-file)
               (compile-file* src-file dest-file))
           (parse-ns src-file dest-file))
         (throw (java.io.FileNotFoundException. (str "The file " src " does not exist.")))))))

(comment
  ;; flex compile-file
  (do
    (compile-file "/tmp/hello.cljs" "/tmp/something.js")
    (slurp "/tmp/hello.js")

    (compile-file "/tmp/somescript.cljs")
    (slurp "/tmp/somescript.js")))

(defn path-seq
  [file-str]
  (->> java.io.File/separator
       java.util.regex.Pattern/quote
       re-pattern
       (string/split file-str)))

(defn to-path
  ([parts]
     (to-path parts java.io.File/separator))
  ([parts sep]
     (apply strict-str (interpose sep parts))))

(defn to-target-file
  "Given the source root directory, the output target directory and
  file under the source root, produce the target file."
  [^java.io.File dir ^String target ^java.io.File file]
  (let [dir-path (path-seq (.getAbsolutePath dir))
        file-path (path-seq (.getAbsolutePath file))
        relative-path (drop (count dir-path) file-path)
        parents (butlast relative-path)
        parent-file (java.io.File. ^String (to-path (cons target parents)))]
    (java.io.File. parent-file ^String (rename-to-scm (last relative-path)))))

(defn cljs-files-in
  "Return a sequence of all .cljs files in the given directory."
  [dir]
  (filter #(let [name (.getName ^java.io.File %)]
             (and (.endsWith name ".cljs")
                  (not= \. (first name))
                  (not (contains? cljs-reserved-file-names name))))
          (file-seq dir)))

(defn compile-root
  "Looks recursively in src-dir for .cljs files and compiles them to
   .js files. If target-dir is provided, output will go into this
   directory mirroring the source directory structure. Returns a list
   of maps containing information about each file which was compiled
   in dependency order."
  ([src-dir]
     (compile-root src-dir "out"))
  ([src-dir target-dir]
     (let [src-dir-file (io/file src-dir)]
       (loop [cljs-files (cljs-files-in src-dir-file)
              output-files []]
         (if (seq cljs-files)
           (let [cljs-file (first cljs-files)
                 output-file ^java.io.File (to-target-file src-dir-file target-dir cljs-file)
                 ns-info (compile-file cljs-file output-file)]
             (recur (rest cljs-files) (conj output-files (assoc ns-info :file-name (.getPath output-file)))))
           output-files)))))

(comment
  ;; compile-root
  ;; If you have a standard project layout with all file in src
  (compile-root "src")
  ;; will produce a mirrored directory structure under "out" but all
  ;; files will be compiled to js.
  )

(comment

;;the new way - use the REPL!!
(require '[cljs.compiler :as comp])
(def repl-env (comp/repl-env))
(comp/repl repl-env)
;having problems?, try verbose mode
(comp/repl repl-env :verbose true)
;don't forget to check for uses of undeclared vars
(comp/repl repl-env :warn-on-undeclared true)

(test-stuff)
(+ 1 2 3)
([ 1 2 3 4] 2)
({:a 1 :b 2} :a)
({1 1 2 2} 1)
(#{1 2 3} 2)
(:b {:a 1 :b 2})
('b '{:a 1 b 2})

(extend-type number ISeq (-seq [x] x))
(seq 42)
;(aset cljs.core.ISeq "number" true)
;(aget cljs.core.ISeq "number")
(satisfies? ISeq 42)
(extend-type nil ISeq (-seq [x] x))
(satisfies? ISeq nil)
(seq nil)

(extend-type default ISeq (-seq [x] x))
(satisfies? ISeq true)
(seq true)

(test-stuff)

(array-seq [])
(defn f [& etc] etc)
(f)

(in-ns 'cljs.core)
;;hack on core


(deftype Foo [a] IMeta (-meta [_] (fn [] a)))
((-meta (Foo. 42)))

;;OLD way, don't you want to use the REPL?
(in-ns 'cljs.compiler)
(import '[javax.script ScriptEngineManager])
(def jse (-> (ScriptEngineManager.) (.getEngineByName "JavaScript")))
(.eval jse cljs.compiler/bootjs)
(def envx {:ns (@namespaces 'cljs.user) :context :expr :locals '{ethel {:name ethel__123 :init nil}}})
(analyze envx nil)
(analyze envx 42)
(analyze envx "foo")
(analyze envx 'fred)
(analyze envx 'fred.x)
(analyze envx 'ethel)
(analyze envx 'ethel.x)
(analyze envx 'my.ns/fred)
(analyze envx 'your.ns.fred)
(analyze envx '(if test then else))
(analyze envx '(if test then))
(analyze envx '(and fred ethel))
(analyze (assoc envx :context :statement) '(def test "fortytwo" 42))
(analyze (assoc envx :context :expr) '(fn* ^{::fields [a b c]} [x y] a y x))
(analyze (assoc envx :context :statement) '(let* [a 1 b 2] a))
(analyze (assoc envx :context :statement) '(defprotocol P (bar [a]) (baz [b c])))
(analyze (assoc envx :context :statement) '(. x y))
(analyze envx '(fn foo [x] (let [x 42] (js* "~{x}['foobar']"))))

(analyze envx '(ns fred (:require [your.ns :as yn]) (:require-macros [clojure.core :as core])))
(defmacro js [form]
  `(emit (ana/analyze {:ns (@ana/namespaces 'cljs.user) :context :statement :locals {}} '~form)))

(defn jscapture [form]
  "just grabs the js, doesn't print it"
  (with-out-str
    (emit (analyze {:ns (@namespaces 'cljs.user) :context :expr :locals {}} form))))

(defn jseval [form]
  (let [js (jscapture form)]
    ;;(prn js)
    (.eval jse (str "print(" js ")"))))

;; from closure.clj
(optimize (jscapture '(defn foo [x y] (if true 46 (recur 1 x)))))

(js (if a b c))
(js (def x 42))
(js (defn foo [a b] a))
(js (do 1 2 3))
(js (let [a 1 b 2 a b] a))

(js (ns fred (:require [your.ns :as yn]) (:require-macros [cljs.core :as core])))

(js (def foo? (fn* ^{::fields [a? b c]} [x y] (if true a? (recur 1 x)))))
(js (def foo (fn* ^{::fields [a b c]} [x y] (if true a (recur 1 x)))))
(js (defn foo [x y] (if true x y)))
(jseval '(defn foo [x y] (if true x y)))
(js (defn foo [x y] (if true 46 (recur 1 x))))
(jseval '(defn foo [x y] (if true 46 (recur 1 x))))
(jseval '(foo 1 2))
(js (and fred ethel))
(jseval '(ns fred (:require [your.ns :as yn]) (:require-macros [cljs.core :as core])))
(js (def x 42))
(jseval '(def x 42))
(jseval 'x)
(jseval '(if 42 1 2))
(jseval '(or 1 2))
(jseval '(fn* [x y] (if true 46 (recur 1 x))))
(.eval jse "print(test)")
(.eval jse "print(cljs.user.Foo)")
(.eval jse  "print(cljs.user.Foo = function (){\n}\n)")
(js (def fred 42))
(js (deftype* Foo [a b-foo c]))
(jseval '(deftype* Foo [a b-foo c]))
(jseval '(. (new Foo 1 2 3) b-foo))
(js (. (new Foo 1 2 3) b))
(.eval jse "print(new cljs.user.Foo(1, 42, 3).b)")
(.eval jse "(function (x, ys){return Array.prototype.slice.call(arguments, 1);})(1,2)[0]")

(macroexpand-1 '(cljs.core/deftype Foo [a b c] Fred (fred [x] a) (fred [x y] b) (ethel [x] c) Ethel (foo [] d)))
(-> (macroexpand-1 '(cljs.core/deftype Foo [a b c] Fred (fred [x] a) (fred [x y] b) (ethel [x] c) Ethel (foo [] d)))
    last last last first meta)

(macroexpand-1 '(cljs.core/extend-type Foo Fred (fred ([x] a) ([x y] b)) (ethel ([x] c)) Ethel (foo ([] d))))
(js (new foo.Bar 65))
(js (defprotocol P (bar [a]) (baz [b c])))
(js (. x y))
(js (. "fred" (y)))
(js (. x y 42 43))
(js (.. a b c d))
(js (. x (y 42 43)))
(js (fn [x] x))
(js (fn ([t] t) ([x y] y) ([ a b & zs] b)))

(js (. (fn foo ([t] t) ([x y] y) ([a b & zs] b)) call nil 1 2))
(js (fn foo
      ([t] t)
      ([x y] y)
      ([ a b & zs] b)))

(js ((fn foo
       ([t] (foo t nil))
       ([x y] y)
       ([ a b & zs] b)) 1 2 3))


(jseval '((fn foo ([t] t) ([x y] y) ([ a b & zs] zs)) 12 13 14 15))

(js (defn foo [this] this))

(js (defn foo [a b c & ys] ys))
(js ((fn [x & ys] ys) 1 2 3 4))
(jseval '((fn [x & ys] ys) 1 2 3 4))
(js (cljs.core/deftype Foo [a b c] Fred (fred [x] a) (fred [x y] a)  (ethel [x] c) Ethel (foo [] d)))
(jseval '(cljs.core/deftype Foo [a b c] Fred (fred [x] a) (fred [x y] a)  (ethel [x] c) Ethel (foo [] d)))

(js (do
           (defprotocol Proto (foo [this]))
           (deftype Type [a] Proto (foo [this] a))
           (foo (new Type 42))))

(jseval '(do
           (defprotocol P-roto (foo? [this]))
           (deftype T-ype [a] P-roto (foo? [this] a))
           (foo? (new T-ype 42))))

(js (def x (fn foo [x] (let [x 42] (js* "~{x}['foobar']")))))
(js (let [a 1 b 2 a b] a))

(doseq [e '[nil true false 42 "fred" fred ethel my.ns/fred your.ns.fred
            (if test then "fooelse")
            (def x 45)
            (do x y y)
            (fn* [x y] x y x)
            (fn* [x y] (if true 46 (recur 1 x)))
            (let* [a 1 b 2 a a] a b)
            (do "do1")
            (loop* [x 1 y 2] (if true 42 (do (recur 43 44))))
            (my.foo 1 2 3)
            (let* [a 1 b 2 c 3] (set! y.s.d b) (new fred.Ethel a b c))
            (let [x (do 1 2 3)] x)
            ]]
  (->> e (analyze envx) emit)
  (newline)))
