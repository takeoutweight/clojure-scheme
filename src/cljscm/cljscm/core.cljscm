;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljscm.core)

;hints for generating fast-path dispatch for known core implementors. Falls back to vtable lookup.
;can be auto-generated from an analysis pass: (do (ana/analyze-file "cljs/core.cljs") (:proto-implementers @cljscm.analyzer/namespaces))
;(cljscm.core/add-protocol-hints!)
(cljscm.core/add-protocol-hints!
 {cljscm.core/IMeta [cljscm.core/Symbol+ cljscm.core/PersistentQueue cljscm.core/PersistentArrayMap cljscm.core/Atom cljscm.core/Nil cljscm.core/EmptyList cljscm.core/Subvec cljscm.core/Range cljscm.core/PersistentQueueSeq cljscm.core/RedNode cljscm.core/ChunkedCons cljscm.core/Vector cljscm.core/PersistentVector cljscm.core/ArrayNodeSeq cljscm.core/RSeq cljscm.core/PersistentTreeSet cljscm.core/LazySeq cljscm.core/BlackNode cljscm.core/PersistentHashMap cljscm.core/NodeSeq cljscm.core/Cons cljscm.core/PersistentTreeMapSeq cljscm.core/PersistentTreeMap cljscm.core/PersistentHashSet],
  cljscm.core/IFn [cljscm.core/PersistentArrayMap cljscm.core/MultiFn cljscm.core/Subvec cljscm.core/Pair cljscm.core/TransientVector cljscm.core/RedNode cljscm.core/Vector cljscm.core/Keyword cljscm.core/PersistentVector cljscm.core/String cljscm.core/Table cljscm.core/PersistentTreeSet cljscm.core/TransientHashSet cljscm.core/BlackNode cljscm.core/PersistentHashMap cljscm.core/Symbol cljscm.core/PersistentTreeMap cljscm.core/Array cljscm.core/PersistentHashSet],
  cljscm.core/IEquiv [cljscm.core/PersistentQueue cljscm.core/PersistentArrayMap cljscm.core/Atom cljscm.core/Nil cljscm.core/EmptyList cljscm.core/MultiFn cljscm.core/Subvec cljscm.core/Range cljscm.core/Number cljscm.core/Char cljscm.core/Pair cljscm.core/PersistentQueueSeq cljscm.core/IndexedSeq cljscm.core/ChunkedSeq cljscm.core/RedNode cljscm.core/ChunkedCons cljscm.core/Vector cljscm.core/Keyword cljscm.core/PersistentVector cljscm.core/ArrayNodeSeq cljscm.core/String cljscm.core/Table cljscm.core/RSeq cljscm.core/Class cljscm.core/PersistentTreeSet cljscm.core/Procedure cljscm.core/NeverEquiv cljscm.core/LazySeq cljscm.core/BlackNode cljscm.core/PersistentHashMap cljscm.core/NodeSeq cljscm.core/Cons cljscm.core/UUID cljscm.core/PersistentTreeMapSeq cljscm.core/Symbol cljscm.core/PersistentTreeMap cljscm.core/Null cljscm.core/Array cljscm.core/Boolean cljscm.core/PersistentHashSet],
  cljscm.core/ICollection [cljscm.core/PersistentQueue cljscm.core/PersistentArrayMap cljscm.core/Nil cljscm.core/EmptyList cljscm.core/Subvec cljscm.core/Range cljscm.core/Pair cljscm.core/PersistentQueueSeq cljscm.core/IndexedSeq cljscm.core/ChunkedSeq cljscm.core/RedNode cljscm.core/ChunkedCons cljscm.core/Vector cljscm.core/PersistentVector cljscm.core/ArrayNodeSeq cljscm.core/Table cljscm.core/RSeq cljscm.core/PersistentTreeSet cljscm.core/LazySeq cljscm.core/BlackNode cljscm.core/PersistentHashMap cljscm.core/NodeSeq cljscm.core/Cons cljscm.core/PersistentTreeMapSeq cljscm.core/PersistentTreeMap cljscm.core/Null cljscm.core/Array cljscm.core/PersistentHashSet],
  cljscm.core/IVector [cljscm.core/Subvec cljscm.core/RedNode cljscm.core/Vector cljscm.core/PersistentVector cljscm.core/BlackNode cljscm.core/Array],
  cljscm.core/INode [cljscm.core/BitmapIndexedNode cljscm.core/HashCollisionNode cljscm.core/ArrayNode],
  cljscm.core/IPairable [cljscm.core/Symbol+ cljscm.core/EmptyList cljscm.core/Subvec cljscm.core/Pair cljscm.core/IndexedSeq cljscm.core/Vector cljscm.core/Cons cljscm.core/Null cljscm.core/Array],
  cljscm.core/Fn [cljscm.core/Procedure],
  cljscm.core/IHash [cljscm.core/PersistentQueue cljscm.core/PersistentArrayMap cljscm.core/Atom cljscm.core/Nil cljscm.core/EmptyList cljscm.core/MultiFn cljscm.core/Subvec cljscm.core/Range cljscm.core/Number cljscm.core/Char cljscm.core/Pair cljscm.core/PersistentQueueSeq cljscm.core/IndexedSeq cljscm.core/ChunkedSeq cljscm.core/RedNode cljscm.core/ChunkedCons cljscm.core/Vector cljscm.core/Keyword cljscm.core/PersistentVector cljscm.core/ArrayNodeSeq cljscm.core/String cljscm.core/Table cljscm.core/RSeq cljscm.core/Class cljscm.core/PersistentTreeSet cljscm.core/Procedure cljscm.core/LazySeq cljscm.core/BlackNode cljscm.core/PersistentHashMap cljscm.core/NodeSeq cljscm.core/Cons cljscm.core/UUID cljscm.core/PersistentTreeMapSeq cljscm.core/Symbol cljscm.core/PersistentTreeMap cljscm.core/Null cljscm.core/Boolean cljscm.core/PersistentHashSet],
  cljscm.core/IIndexed [cljscm.core/Nil cljscm.core/EmptyList cljscm.core/Subvec cljscm.core/Range cljscm.core/Pair cljscm.core/IndexedSeq cljscm.core/TransientVector cljscm.core/RedNode cljscm.core/Vector cljscm.core/PersistentVector cljscm.core/String cljscm.core/ArrayChunk cljscm.core/LazySeq cljscm.core/BlackNode cljscm.core/Cons cljscm.core/Null cljscm.core/Array],
  cljscm.core/IWriter [cljscm.core/StringBufferWriter],
  cljscm.core/IChunkedSeq [cljscm.core/ChunkedSeq cljscm.core/ChunkedCons],
  cljscm.core/IEmptyableCollection [cljscm.core/PersistentQueue cljscm.core/PersistentArrayMap cljscm.core/Nil cljscm.core/EmptyList cljscm.core/Subvec cljscm.core/Range cljscm.core/Pair cljscm.core/PersistentQueueSeq cljscm.core/IndexedSeq cljscm.core/ChunkedSeq cljscm.core/RedNode cljscm.core/ChunkedCons cljscm.core/Vector cljscm.core/PersistentVector cljscm.core/ArrayNodeSeq cljscm.core/Table cljscm.core/RSeq cljscm.core/PersistentTreeSet cljscm.core/LazySeq cljscm.core/BlackNode cljscm.core/PersistentHashMap cljscm.core/NodeSeq cljscm.core/Cons cljscm.core/PersistentTreeMapSeq cljscm.core/PersistentTreeMap cljscm.core/Null cljscm.core/PersistentHashSet],
  cljscm.core/ICounted [cljscm.core/PersistentQueue cljscm.core/PersistentArrayMap cljscm.core/Nil cljscm.core/ChunkBuffer cljscm.core/EmptyList cljscm.core/Subvec cljscm.core/Range cljscm.core/Pair cljscm.core/IndexedSeq cljscm.core/TransientArrayMap cljscm.core/TransientVector cljscm.core/RedNode cljscm.core/Vector cljscm.core/PersistentVector cljscm.core/String cljscm.core/Table cljscm.core/RSeq cljscm.core/PersistentTreeSet cljscm.core/TransientHashSet cljscm.core/ArrayChunk cljscm.core/BlackNode cljscm.core/PersistentHashMap cljscm.core/Cons cljscm.core/TransientHashMap cljscm.core/PersistentTreeMapSeq cljscm.core/PersistentTreeMap cljscm.core/Null cljscm.core/Array cljscm.core/PersistentHashSet],
  cljscm.core/IKVReduce [cljscm.core/PersistentArrayMap cljscm.core/BitmapIndexedNode cljscm.core/HashCollisionNode cljscm.core/RedNode cljscm.core/PersistentVector cljscm.core/ArrayNode cljscm.core/BlackNode cljscm.core/PersistentHashMap cljscm.core/PersistentTreeMap],
  cljscm.core/ITransientVector [cljscm.core/TransientVector],
  cljscm.core/ISorted [cljscm.core/PersistentTreeSet cljscm.core/PersistentTreeMap],
  cljscm.core/IRedBlackNode [cljscm.core/RedNode cljscm.core/BlackNode],
  cljscm.core/IWatchable [cljscm.core/Atom],
  cljscm.core/ITransientSet [cljscm.core/TransientHashSet],
  cljscm.core/IEditableCollection [cljscm.core/PersistentArrayMap cljscm.core/PersistentVector cljscm.core/PersistentHashMap cljscm.core/PersistentHashSet],
  cljscm.core/IStringable [cljscm.core/Char cljscm.core/String cljscm.core/StringBufferWriter],
  cljscm.core/IAssociative [cljscm.core/PersistentArrayMap cljscm.core/Nil cljscm.core/Subvec cljscm.core/RedNode cljscm.core/Vector cljscm.core/PersistentVector cljscm.core/Table cljscm.core/BlackNode cljscm.core/PersistentHashMap cljscm.core/PersistentTreeMap cljscm.core/Array],
  cljscm.core/ISeqable [cljscm.core/Cons cljscm.core/LazySeq cljscm.core/Range cljscm.core/PersistentVector cljscm.core/PersistentQueue cljscm.core/PersistentArrayMap cljscm.core/Nil cljscm.core/EmptyList cljscm.core/Subvec cljscm.core/Pair cljscm.core/PersistentQueueSeq cljscm.core/IndexedSeq cljscm.core/ChunkedSeq cljscm.core/RedNode cljscm.core/ChunkedCons cljscm.core/Vector cljscm.core/ArrayNodeSeq cljscm.core/String cljscm.core/Table cljscm.core/RSeq cljscm.core/PersistentTreeSet cljscm.core/BlackNode cljscm.core/PersistentHashMap cljscm.core/NodeSeq cljscm.core/PersistentTreeMapSeq cljscm.core/PersistentTreeMap cljscm.core/Null cljscm.core/Array cljscm.core/PersistentHashSet],
  cljscm.core/ITransientMap [cljscm.core/TransientArrayMap cljscm.core/TransientHashMap],
  cljscm.core/IMultiFn [cljscm.core/MultiFn],
  cljscm.core/IList [cljscm.core/EmptyList cljscm.core/Pair cljscm.core/Cons],
  cljscm.core/ISet [cljscm.core/Nil cljscm.core/PersistentTreeSet cljscm.core/PersistentHashSet],
  cljscm.core/IMapEntry [cljscm.core/RedNode cljscm.core/PersistentVector cljscm.core/BlackNode],
  cljscm.core/IReversible [cljscm.core/IndexedSeq cljscm.core/PersistentVector cljscm.core/PersistentTreeSet cljscm.core/PersistentTreeMap],
  cljscm.core/ASeq [cljscm.core/Pair cljscm.core/IndexedSeq cljscm.core/ChunkedSeq cljscm.core/ChunkedCons cljscm.core/Cons],
  cljscm.core/IPrintWithWriter [cljscm.core/PersistentQueue cljscm.core/PersistentArrayMap cljscm.core/Atom cljscm.core/Nil cljscm.core/EmptyList cljscm.core/Subvec cljscm.core/Error cljscm.core/Range cljscm.core/Number cljscm.core/Char cljscm.core/Pair cljscm.core/IndexedSeq cljscm.core/ChunkedSeq cljscm.core/RedNode cljscm.core/ChunkedCons cljscm.core/Vector cljscm.core/Keyword cljscm.core/PersistentVector cljscm.core/ArrayNodeSeq cljscm.core/String cljscm.core/Table cljscm.core/RSeq cljscm.core/PersistentTreeSet cljscm.core/Procedure cljscm.core/LazySeq cljscm.core/BlackNode cljscm.core/PersistentHashMap cljscm.core/NodeSeq cljscm.core/Cons cljscm.core/UUID cljscm.core/PersistentTreeMapSeq cljscm.core/Symbol cljscm.core/Symbol+ cljscm.core/PersistentTreeMap cljscm.core/Null cljscm.core/Array cljscm.core/Boolean cljscm.core/PersistentHashSet cljscm.core/Class],
  cljscm.core/ISequential [cljscm.core/PersistentQueue cljscm.core/EmptyList cljscm.core/Subvec cljscm.core/Range cljscm.core/Pair cljscm.core/PersistentQueueSeq cljscm.core/IndexedSeq cljscm.core/ChunkedSeq cljscm.core/RedNode cljscm.core/ChunkedCons cljscm.core/Vector cljscm.core/PersistentVector cljscm.core/ArrayNodeSeq cljscm.core/RSeq cljscm.core/LazySeq cljscm.core/BlackNode cljscm.core/NodeSeq cljscm.core/Cons cljscm.core/PersistentTreeMapSeq cljscm.core/Null cljscm.core/Array],
  cljscm.core/IDeref [cljscm.core/Atom cljscm.core/Reduced cljscm.core/Delay],
  cljscm.core/IComparable [cljscm.core/PersistentVector],
  cljscm.core/INext [cljscm.core/LazySeq cljscm.core/Cons cljscm.core/Nil cljscm.core/EmptyList cljscm.core/Range cljscm.core/Pair cljscm.core/IndexedSeq cljscm.core/ChunkedSeq],
  cljscm.core/IReduce [cljscm.core/LazySeq cljscm.core/Cons cljscm.core/Nil cljscm.core/EmptyList cljscm.core/Subvec cljscm.core/Range cljscm.core/Pair cljscm.core/IndexedSeq cljscm.core/RedNode cljscm.core/Vector cljscm.core/PersistentVector cljscm.core/String cljscm.core/Table cljscm.core/ArrayChunk cljscm.core/BlackNode cljscm.core/Null cljscm.core/Array cljscm.core/PersistentHashSet],
  cljscm.core/ITransientAssociative [cljscm.core/TransientArrayMap cljscm.core/TransientVector cljscm.core/TransientHashMap],
  cljscm.core/IPending [cljscm.core/Delay],
  cljscm.core/IChunk [cljscm.core/ArrayChunk],
  cljscm.core/IMap [cljscm.core/PersistentArrayMap cljscm.core/Nil cljscm.core/Table cljscm.core/PersistentHashMap cljscm.core/PersistentTreeMap],
  cljscm.core/ITransientCollection [cljscm.core/TransientArrayMap cljscm.core/TransientVector cljscm.core/TransientHashSet cljscm.core/TransientHashMap],
  cljscm.core/IStack [cljscm.core/PersistentQueue cljscm.core/Nil cljscm.core/EmptyList cljscm.core/Subvec cljscm.core/Pair cljscm.core/RedNode cljscm.core/Vector cljscm.core/PersistentVector cljscm.core/BlackNode cljscm.core/Cons],
  cljscm.core/IWithMeta [cljscm.core/Symbol cljscm.core/Symbol+ cljscm.core/PersistentQueue cljscm.core/PersistentArrayMap cljscm.core/Nil cljscm.core/EmptyList cljscm.core/Subvec cljscm.core/Range cljscm.core/Pair cljscm.core/PersistentQueueSeq cljscm.core/ChunkedSeq cljscm.core/RedNode cljscm.core/ChunkedCons cljscm.core/Vector cljscm.core/PersistentVector cljscm.core/ArrayNodeSeq cljscm.core/Table cljscm.core/RSeq cljscm.core/PersistentTreeSet cljscm.core/LazySeq cljscm.core/BlackNode cljscm.core/PersistentHashMap cljscm.core/NodeSeq cljscm.core/Cons cljscm.core/PersistentTreeMapSeq cljscm.core/PersistentTreeMap cljscm.core/Array cljscm.core/PersistentHashSet],
  cljscm.core/IPrintable [cljscm.core/Nil cljscm.core/Procedure cljscm.core/UUID cljscm.core/Null],
  cljscm.core/IChunkedNext [cljscm.core/ChunkedSeq cljscm.core/ChunkedCons],
  cljscm.core/ILookup [cljscm.core/PersistentArrayMap cljscm.core/Nil cljscm.core/Subvec cljscm.core/TransientArrayMap cljscm.core/TransientVector cljscm.core/RedNode cljscm.core/Vector cljscm.core/PersistentVector cljscm.core/String cljscm.core/Table cljscm.core/PersistentTreeSet cljscm.core/TransientHashSet cljscm.core/BlackNode cljscm.core/PersistentHashMap cljscm.core/TransientHashMap cljscm.core/PersistentTreeMap cljscm.core/Array cljscm.core/PersistentHashSet],
  cljscm.core/ISeq [cljscm.core/LazySeq cljscm.core/Cons cljscm.core/Range cljscm.core/PersistentQueue cljscm.core/Nil cljscm.core/EmptyList cljscm.core/Pair cljscm.core/PersistentQueueSeq cljscm.core/IndexedSeq cljscm.core/ChunkedSeq cljscm.core/ChunkedCons cljscm.core/ArrayNodeSeq cljscm.core/RSeq cljscm.core/NodeSeq cljscm.core/PersistentTreeMapSeq cljscm.core/Null]})

;(def *unchecked-if* false)

(def
  ^{:doc "bound in a repl thread to the most recent value printed"}
  *1)

(def
  ^{:doc "bound in a repl thread to the second most recent value printed"}
  *2)

(def
  ^{:doc "bound in a repl thread to the third most recent value printed"}
  *3)

(defn ^boolean identical?
  "Tests if 2 arguments are the same object"
  [x y]
  ((scm* {} eq?) x y))

(defn ^boolean nil?
  "Returns true if x is nil, false otherwise."
  [x]
  (identical? x nil))

(defn ^boolean not
  "Returns true if x is logical false, false otherwise."
  [x] (if x false true))

(def
  ^{:doc "When compiled for a command-line target, whatever
  function *main-fn* is set to will be called with the command-line
  argv as arguments"}
  *main-cli-fn* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arrays ;;;;;;;;;;;;;;;;

#_(defn aclone
  "Returns a javascript array, cloned from the passed in array"
  [a]
  (scm* [a] (vector-copy a)))

(defn array
  "Creates a raw scheme \"array\" (a mutable vector, distinct from persistent clojure Vector)
@param {...*} var_args" ;;array is a special case, don't emulate this doc string
  ([] (scm* [] (vector)))
  ([& var-args] ;; [& items]
     (scm* [var-args] (apply vector var-args))))

(defn make-array
  ([size] (scm* {:size size :void nil} (make-vector :size :void)))
  ([type size]
     (make-array size)))

(declare apply)

(defn aget
  "Returns the value at the index."
  ([array i]
     ((scm* {} vector-ref) array i))
  ([array i & idxs]
     (apply aget (aget array i) idxs)))

(defn aset
  "Sets the value at the index."
  [array i val]
  ((scm* {} vector-set!) array i val)
  val)

(defn alength
  "Returns the length of the array. Works on arrays of all types."
  [array]
  ((scm* {} vector-length) array))

(declare reduce)

(defn into-array
  ([aseq]
     (into-array nil aseq))
  ([type aseq]
     (apply array aseq)))

(defn char
  [n]
  (scm* [n] (integer->char n)))

(defn char?
  "Returns true if x is a char, false otherwise."
  [x] (scm-boolean* [x] (char? x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;; core protocols ;;;;;;;;;;;;;
(scm* {} (define cljscm.core/protocol-impls (make-table)))
(scm* {} (define cljscm.core/foreign-tags (make-table)))
;all proto defns need to be able to throw errors with str msgs- but errors themselves can implement protocols
;(scm* {} (define make-cljscm.core/Error))
;(declare str)

(defprotocol Fn
  "Marker protocol")

(defprotocol IFn
  (-invoke [this args]))

(scm* {} (include "polymorphic-apply.scm")) ;to trap non-proc exceptions and calls -invoke

(defprotocol ICounted
  (-count [coll] "constant time count"))

(defprotocol IEmptyableCollection
  (-empty [coll]))

(defprotocol ICollection
  (-conj [coll o]))

#_(defprotocol IOrdinal
    (-index [coll]))

(defprotocol IIndexed
  (-nth [coll n] [coll n not-found]))

(defprotocol ASeq)

(defprotocol ISeq
  (-first [coll])
  (-rest [coll]))

(defprotocol INext
  (-next [coll]))

(defprotocol ILookup
  (-lookup [o k] [o k not-found]))

(defprotocol IAssociative
  (-contains-key? [coll k])
  #_(-entry-at [coll k])
  (-assoc [coll k v]))

(defprotocol IMap
  #_(-assoc-ex [coll k v])
  (-dissoc [coll k]))

(defprotocol IMapEntry
  (-key [coll])
  (-val [coll]))

(defprotocol ISet
  (-disjoin [coll v]))

(defprotocol IStack
  (-peek [coll])
  (-pop [coll]))

(defprotocol IVector
  (-assoc-n [coll n val]))

(defprotocol IDeref
 (-deref [o]))

(defprotocol IDerefWithTimeout
  (-deref-with-timeout [o msec timeout-val]))

(defprotocol IMeta
  (-meta [o]))

(defprotocol IWithMeta
  (-with-meta [o meta]))

(defprotocol IReduce
  (-reduce [coll f] [coll f start]))

(defprotocol IKVReduce
  (-kv-reduce [coll f init]))

(defprotocol IEquiv
  (-equiv [o other]))

(defprotocol IHash
  (-hash [o]))

(defprotocol ISeqable
  (-seq [o]))

(defprotocol ISequential
  "Marker interface indicating a persistent collection of sequential items")

(defprotocol IList
  "Marker interface indicating a persistent list")

(defprotocol IRecord
  "Marker interface indicating a record object")

(defprotocol IReversible
  (-rseq [coll]))

(defprotocol ISorted
  (-sorted-seq [coll ascending?])
  (-sorted-seq-from [coll k ascending?])
  (-entry-key [coll entry])
  (-comparator [coll]))

(defprotocol ^:deprecated IPrintable
  "Do not use this.  It is kept for backwards compatibility with existing
   user code that depends on it, but it has been superceded by IPrintWithWriter
   User code that depends on this should be changed to use -pr-writer instead."
  (-pr-seq [o opts]))

(defprotocol IWriter
  (-write [writer s])
  (-flush [writer]))

(defprotocol IStringable
  (-toString [o]))

(defprotocol IPrintWithWriter
  "The old IPrintable protocol's implementation consisted of building a giant
   list of strings to concatenate.  This involved lots of concat calls,
   intermediate vectors, and lazy-seqs, and was very slow in some older JS
   engines.  IPrintWithWriter implements printing via the IWriter protocol, so it
   be implemented efficiently in terms of e.g. a StringBuffer append."
  (-pr-writer [o writer opts]))

(defprotocol IPending
  (-realized? [d]))

(defprotocol IWatchable
  (-notify-watches [this oldval newval])
  (-add-watch [this key f])
  (-remove-watch [this key]))

(defprotocol IEditableCollection
  (-as-transient [coll]))

(defprotocol ITransientCollection
  (-conj! [tcoll val])
  (-persistent! [tcoll]))

(defprotocol ITransientAssociative
  (-assoc! [tcoll key val]))

(defprotocol ITransientMap
  (-dissoc! [tcoll key]))

(defprotocol ITransientVector
  (-assoc-n! [tcoll n val])
  (-pop! [tcoll]))

(defprotocol ITransientSet
  (-disjoin! [tcoll v]))

(defprotocol IComparable
  (-compare [x y]))

(defprotocol IChunk
  (-drop-first [coll]))

(defprotocol IChunkedSeq
  (-chunked-first [coll])
  (-chunked-rest [coll]))

(defprotocol IChunkedNext
  (-chunked-next [coll]))

(defprotocol INode
  (-inode-assoc [inode shift hash key val added-leaf?])
  (-inode-without [inode shift hash key])
  (-inode-lookup [inode shift hash key not-found])
  (-inode-find [inode shift hash key not-found])
  (-inode-seq [inode])
  (-ensure-editable [inode e])
  (-inode-assoc! [inode edit shift hash key val added-leaf?])
  (-inode-without! [inode edit shift hash key removed-leaf?]))

(defprotocol IRedBlackNode
  (-add-left [node ins])
  (-add-right [node ins])
  (-remove-left [node del])
  (-remove-right [node del])
  (-blacken [node])
  (-redden [node])
  (-balance-left [node parent])
  (-balance-right [node parent])
  (-replace [node key val left right]))

(defprotocol IPairable
  (-pair [coll]))

(defn pair [coll]
  (if (satisfies? IPairable coll)
    (-pair coll)
    (if (seq coll)
      ((scm* {} cons) (first coll) (pair (rest coll)))
      ())))

(defn pair-item
  [o]
  (if (satisfies? IPairable o)
    (-pair o)
    o))

(defn pair-recursive [coll]
  (cond
;have to worry about improper lists.
    (scm* [coll] (and (pair? coll) (not (list? coll))))
    , (let [fst (scm* [coll] (car coll))
            snd (scm* [coll] (cdr coll))]
        ((scm* {} cons)
         (if (coll? fst) (pair-recursive fst) (pair-item fst))
         (if (coll? snd) (pair-recursive snd) (pair-item snd))))
    (seq coll) (let [[fst & rst] coll]
                 ((scm* {} cons)
                  (if (coll? fst) (pair-recursive fst) (pair-item fst))
                  (pair-recursive rst)))
    :else ()))

#_(defn pair-recursive [coll]
  (if (seq coll)
    ((scm* {} cons)
     (if (coll? (first coll)) (pair-recursive (first coll)) (first coll))
     (pair-recursive (rest coll)))
    ()))

(defn record-ref [obj field]
  (if (and (identical? 1 (scm-type-idx obj))
           (identical? 4 (scm-subtype-idx obj)))
    (let [field-params (pair (scm-unsafe-vector-ref (scm-unsafe-vector-ref obj 0) 5))
          search (scm-boolean* [field field-params] (memq field field-params))]
      (if search
        (let [slot (inc (/ (- (count field-params) (count search)) 3))]
          (scm-unsafe-vector-ref obj slot))
        (throw (Error. (str "Field not defined: " field " on " field-params)))))
    (throw (Error. (str "Not a deftype: " obj)))))

(defn record-set! [obj field val]
  (if (and (identical? 1 (scm-type-idx obj))
           (identical? 4 (scm-subtype-idx obj)))
    (let [field-params (pair (scm-unsafe-vector-ref (scm-unsafe-vector-ref obj 0) 5))
          search (scm-boolean* [field field-params] (memq field field-params))]
      (if search
        (let [slot (inc (/ (- (count field-params) (count search)) 3))]
          (scm-unsafe-vector-set! obj slot val))
        (throw (Error. (str "Field not defined: " field " on " field-params)))))
    (throw (Error. (str "Not a deftype: " obj)))))

#_(scm-str* "(define (cljscm.core/record-ref obj field)
           (if (and (eqv? (##type obj) 1) (eqv? (##subtype obj) 4))
             (let* ((field-params (vector->list (##vector-ref (##vector-ref obj 0) 5)))
                    (search (memq field field-params)))
                   (if search 
                     (##vector-ref obj (+ (/ (- (length field-params) (length search)) 3) 1))
                     (raise (string-append \"No field: \" (symbol->string field)))))
             (raise (string-append \"Not a record: \" (object->string obj)))))")

#_(scm-str* "(define (cljscm.core/record-set! obj field val)
           (if (and (eqv? (##type obj) 1) (eqv? (##subtype obj) 4))
             (let* ((field-params (vector->list (##vector-ref (##vector-ref obj 0) 5)))
                    (search (memq field field-params)))
                   (if search
                     (##vector-set! obj (+ (/ (- (length field-params) (length search)) 3) 1) val)
                     (raise (string-append \"No field: \" (symbol->string field)))))
             (raise (string-append \"Not a record: \" (object->string obj)))))")

;;;;;;;;;;;;;;;;;;; fundamentals ;;;;;;;;;;;;;;;
;basic Scheme types-- do not instantiate.
(deftype Number [])
(deftype Pair [])
(deftype Boolean [])
(deftype Nil [])
(deftype Null [])
(deftype Char [])
(deftype Array [])
(deftype Symbol [])
(deftype Keyword [])
(deftype Procedure [])
(deftype String [])

(defn type [x]
  (case (cljscm.core/scm-type-idx x)
    0 Number ;Fixnum
    3 Pair
    2 (case x
        (true false) Boolean
        nil Nil ;we hijack #!void to map to Clojure's nil
        (scm* [x] ()) Null            ;raw scheme null = empty list.
        (cond
          (scm* [x] (char? x)) Char
          :else (throw "Can't discern special type")))
    1 (case (cljscm.core/scm-subtype-idx x)
        0 Array ; raw scheme vector
        (2 3 30 31) Number            ;Rational ;Complex ;Flonum, ;Bignum
        4 (scm-unsafe-vector-ref x 0)
        8 Symbol
        9 Keyword
        14 Procedure
        18 (get foreign-type-table (first (scm* [x] (foreign-tags x)))) ;Foreign ptr.
        19 String
        (20 21 22 23 24 25 26 27 28 29) Array ;Various numerically-typed scheme vectors
        )))

(defn ^seq seq
  "Returns a seq on the collection. If the collection is
  empty, returns nil.  (seq nil) returns nil. seq also works on
  Strings."
  [coll]
  (when-not (nil? coll)
    (if (satisfies? ASeq coll)
      coll
      (-seq coll))))

(defn first
  "Returns the first item in the collection. Calls seq on its
  argument. If coll is nil, returns nil."
  [coll]
  (when-not (nil? coll)
    (if (satisfies? ISeq coll)
      (-first coll)
      (let [s (seq coll)]
        (when-not (nil? s)
          (-first s))))))

(defn ^seq rest
  "Returns a possibly empty seq of the items after the first. Calls seq on its
  argument."
  [coll]
  (if-not (nil? coll)
    (if (satisfies? ISeq coll)
      (-rest coll)
      (let [s (seq coll)]
        (if-not (nil? s)
          (-rest s)
          ())))
    ()))

(defn ^seq next
  "Returns a seq of the items after the first. Calls seq on its
  argument.  If there are no more items, returns nil"
  [coll]
  (when-not (nil? coll)
    (if (satisfies? INext coll)
      (-next coll)
      (seq (rest coll)))))

(defn ^boolean =
  "Equality. Returns true if x equals y, false if not. Compares
  numbers and collections in a type-independent manner.  Clojure's immutable data
  structures define -equiv (and thus =) as a value, not an identity,
  comparison."
  ([x] true)
  ([x y] (or (identical? x y) (scm* [x y] (eqv? x y)) (-equiv x y)))
  ([x y & more]
     (if (= x y)
       (if (next more)
         (recur y (first more) (next more))
         (= y (first more)))
       false)))

(defn ^boolean instance? [t o]
  (identical? t (type o)))

;hijack existing scheme structure types
(def Table (type (scm* {} (make-table))))
(scm* {} (table-set! cljscm.core/protocol-impls cljscm.core/Table (make-table)))
(scm* {} (define (make-cljscm.core/Table) (make-table)))

(def Class (type cljscm.core/Number))
(scm* {} (table-set! cljscm.core/protocol-impls cljscm.core/Class (make-table)))
;(scm* {} (define (cljscm.core/Class? x) (equal? (cljscm.core/type x) )))
(defn Class? [x] (identical? (type x) Class))

(defn scm-equal?-hash [o]
  (scm-boolean* [o] (equal?-hash o)))

(defn identity-hash [o]
  (scm* [o] (object->serial-number o)))
;;;;;;;;;;;;;;;;;;; protocols on primitives ;;;;;;;;
(declare hash-map list equiv-sequential)

;maps to Gambit scheme's #!void
(extend-type Nil
  IEquiv
  (-equiv [_ o] (nil? o))

  ICounted
  (-count [_] 0)

  IEmptyableCollection
  (-empty [_] nil)

  ICollection
  (-conj [_ o] (list o))

;  ^:deprecation-nowarn IPrintable
;  (-pr-seq [o] (list "nil"))

  IIndexed
  (-nth
   ([_ n] nil)
   ([_ n not-found] not-found))

  ISeq
  (-first [_] nil)
  (-rest [_] (list))

  ISeqable
  (-seq [_] nil)

;  IPairable
;  (-pair [_] (scm* [] '()))

  INext
  (-next [_] nil)

  ILookup
  (-lookup
   ([o k] nil)
   ([o k not-found] not-found))

  IAssociative
  (-assoc [_ k v] (hash-map k v))

  IMap
  (-dissoc [_ k] nil)

  ISet
  (-disjoin [_ v] nil)

  IStack
  (-peek [_] nil)
  (-pop [_] nil)

  IMeta
  (-meta [_] nil)

  IWithMeta
  (-with-meta [_ meta] nil)

  IReduce
  (-reduce
    ([_ f] (f))
    ([_ f start] start))

  IHash
  (-hash [o] (scm-equal?-hash o)))

;The scheme empty list '()
(extend-type Null 
  IEquiv
  (-equiv [_ o] (scm* [o] (null? o)))

  ICounted
  (-count [_] 0)

  IEmptyableCollection
  (-empty [_] (scm* [] '()))

  ICollection
  (-conj [_ o] (scm* [o] (list o)))

  IPrintable
  (-pr-seq [o _] (list "#<null>"))

  IIndexed
  (-nth
   ([_ n] nil)
   ([_ n not-found] not-found))

  ISequential
  ISeq
  (-first [_] nil)
  (-rest [_] (scm* [] '()))

  ISeqable
  (-seq [coll] nil)

  IPairable
  (-pair [coll] coll)

  IReduce
  (-reduce
    ([_ f] (f))
    ([_ f start] start))

  IHash
  (-hash [o] (scm-equal?-hash o))
  
  IWithMeta
  (-with-meta [f meta]
    (EmptyList. meta)))

(def List-EMPTY ())

(extend-type Pair
  IList
  
  ASeq
  ISeq
  (-first [coll] (scm* [coll] (car coll)))
  (-rest [coll] (scm* [coll] (cdr coll)))

  INext
  (-next [coll]
    (if (== (scm* {:coll coll} (length coll)) 1)
      nil
      (scm* {:coll coll} (cdr coll))))

  ICollection
  (-conj [coll o] (scm* [coll o] (cons o coll)))

  IEmptyableCollection
  (-empty [coll] (scm* [] '()))

  ISequential
  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  IHash
  (-hash [coll] (hash-coll coll))

  ICounted
  (-count [coll] (scm* [coll] (length coll)))

  IStack
  (-peek [coll] (-first coll))
  (-pop [coll] (-rest coll))

  IIndexed
  (-nth [coll n] (scm* [coll n] (list-ref coll n)))
  (-nth [coll n not-found]
    (if (and (<= 0 n) (< n (-count coll)))
      (scm* [coll n] (list-ref coll n))
      not-found))

  IFn
  (-invoke [coll [k not-found]]
    (if not-found
      (-nth coll k not-found)
      (-nth coll k)))

  ISeqable
  (-seq [coll] coll)

  IPairable
  (-pair [coll] coll)

  IWithMeta
  (-with-meta [coll meta] (Cons. meta (first coll) (rest coll) nil))

  IReduce 
  (-reduce
    ([v f]
       (if (scm* [v] (null? v))
         (f)
         (reduce f (scm* [v] (car v)) (scm* [v] (cdr v)))))
    ([v f start]
       (loop [v v f f start start]
         (if (scm* [v] (null? v))
           start
           (recur (scm* [v] (cdr v)) f (f start (scm* [v] (car v))) ))))))

(extend-type Number
  IEquiv
  (-equiv [x o] (identical? x o))

  IHash
  (-hash [o] (scm-equal?-hash o)))

(extend-type Boolean
  IEquiv
  (-equiv [x o] (identical? x o))
  
  IHash
  (-hash [o] (scm-equal?-hash o)))

(extend-type Keyword
  IEquiv
  (-equiv [x o] (identical? x o))

  IHash
  (-hash [o] (scm-equal?-hash o))

  IFn
  (-invoke [k [coll not-found]]
    (-lookup coll k not-found)))

(deftype Symbol+ [s meta]
  IEquiv
  (-equiv [x o] (if (instance? Symbol+ o)
                  (-equiv s (.-s ^Symbol+ o))
                  (-equiv s o)))

  IHash
  (-hash [o] (scm-equal?-hash s))

  IMeta
  (-meta [o] meta)

  IWithMeta
  (-with-meta [o meta] (Symbol+. s meta))

  IFn
  (-invoke [k [coll not-found]]
    (-lookup coll s not-found))

  IPairable
  (-pair [_] s))

(extend-type Symbol
  IEquiv
  (-equiv [x o] (if (instance? Symbol+ o)
                  (identical? x (.-s ^Symbol+ o))
                  (identical? x o)))

  IHash
  (-hash [o] (scm-equal?-hash o))

  IMeta
  (-meta [o] nil)
    
  IWithMeta
  (-with-meta [s meta] (Symbol+. s meta))

  IFn
  (-invoke [k [coll not-found]]
    (-lookup coll k not-found)))

(extend-type Char
  IEquiv
  (-equiv [x o] (identical? x o))

  IHash
  (-hash [o] (scm-equal?-hash o))

  IStringable
  (-toString [o] ((scm* {} string) o)))

(extend-type Procedure
  Fn
  
  IEquiv
  (-equiv [x o] (identical? x o))
  
  IHash
  (-hash [o] (scm-equal?-hash o))

  IPrintable
  (-pr-seq [a opts]
    [(scm* [a] (object->string a))]))

(extend-type Class
  IEquiv
  (-equiv [x o] (identical? x o))

  IHash
  (-hash [o] (scm-equal?-hash o))

  #_IPrintable
  #_(-pr-seq [a opts]
    ["#<Class " (str (scm-unsafe-vector-ref a 2)) ">"]))

(declare with-meta)

#_(extend-type function
  Fn

  IMeta
  (-meta [_] nil)
  
  IWithMeta
  (-with-meta [f meta]
    (with-meta
      (reify
        Fn
        IFn
        (-invoke [_ & args]
          (apply f args)))
      meta)))

#_(extend-type default
  IHash
  (-hash [o] (scm-equal?-hash o)))

;;this is primitive because & emits call to array-seq
(defn inc
  "Returns a number one greater than num."
  [x] (+ x 1))

(declare deref)


(deftype Error [msg])

(deftype Exception [msg])

(deftype Reduced [val]
  IDeref
  (-deref [o] val))

(defn reduced
  "Wraps x in a way such that a reduce will terminate with the value x"
  [x]
  (Reduced. x))

(defn ^boolean reduced?
  "Returns true if x is the result of a call to reduced"
  [r]
  (instance? Reduced r))

(defn- ci-reduce
  "Accepts any collection which satisfies the ICount and IIndexed protocols and
reduces them without incurring seq initialization"
  ([cicoll f]
     (let [cnt (-count cicoll)]
       (if (zero? cnt)
         (f)
         (loop [val (-nth cicoll 0), n 1]
           (if (< n cnt)
             (let [nval (f val (-nth cicoll n))]
               (if (reduced? nval)
                 @nval
                 (recur nval (inc n))))
             val)))))
  ([cicoll f val]
     (let [cnt (-count cicoll)]
       (loop [val val, n 0]
         (if (< n cnt)
           (let [nval (f val (-nth cicoll n))]
             (if (reduced? nval)
               @nval
               (recur nval (inc n))))
           val))))
  ([cicoll f val idx]
     (let [cnt (-count cicoll)]
       (loop [val val, n idx]
         (if (< n cnt)
           (let [nval (f val (-nth cicoll n))]
             (if (reduced? nval)
               @nval
               (recur nval (inc n))))
           val)))))

(defn- array-reduce
  ([arr f]
     (let [cnt (alength arr)]
       (if (zero? (alength arr))
         (f)
         (loop [val (aget arr 0), n 1]
           (if (< n cnt)
             (let [nval (f val (aget arr n))]
               (if (reduced? nval)
                 @nval
                 (recur nval (inc n))))
             val)))))
  ([arr f val]
     (let [cnt (alength arr)]
       (loop [val val, n 0]
         (if (< n cnt)
           (let [nval (f val (aget arr n))]
             (if (reduced? nval)
               @nval
               (recur nval (inc n))))
           val))))
  ([arr f val idx]
     (let [cnt (alength arr)]
       (loop [val val, n idx]
         (if (< n cnt)
           (let [nval (f val (aget arr n))]
             (if (reduced? nval)
               @nval
               (recur nval (inc n))))
           val)))))

(declare hash-coll cons pr-str RSeq)

(defn ^boolean counted?
  "Returns true if coll implements count in constant time"
  [x] (satisfies? ICounted x))

(defn ^boolean indexed?
  "Returns true if coll implements nth in constant time"
  [x] (satisfies? IIndexed x))

(deftype IndexedSeq [a i]
  ISeqable
  (-seq [this] this)

  IPairable
  (-pair [this] (scm* [a] (vector->list a)))

  ASeq

  ISeq
  (-first [_] (aget a i))
  (-rest [_] (if (< (inc i) (alength a))
               (IndexedSeq. a (inc i))
               (list)))

  INext
  (-next [_] (if (< (inc i) (alength a))
               (IndexedSeq. a (inc i))
               nil))

  ICounted
  (-count [_] (- (alength a) i))

  IIndexed
  (-nth
    ([coll n]
       (let [i (+ n i)]
         (when (< i (alength a))
           (aget a i))))
    ([coll n not-found]
          (let [i (+ n i)]
            (if (< i (alength a))
              (aget a i)
              not-found))))

  ISequential
  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  ICollection
  (-conj [coll o] (cons o coll))

  IEmptyableCollection
  (-empty [coll] List-EMPTY)

  IReduce
  (-reduce
    ([_ f]
       (if (counted? a)
         (ci-reduce a f (aget a i) (inc i))
         (ci-reduce coll f (aget a i) 0)))
    ([_ f start]
       (if (counted? a)
         (ci-reduce a f start i)
         (ci-reduce coll f start 0))))

  IHash
  (-hash [coll] (hash-coll coll))

  IReversible
  (-rseq [coll]
    (let [c (-count coll)]
      (if (pos? c)
        (RSeq. coll (dec c) nil)
        ()))))

(defn prim-seq
  ([prim]
     (prim-seq prim 0))
  ([prim i]
     (when (< i (alength prim))
       (IndexedSeq. prim i))))

(defn array-seq
  ([array]
     (prim-seq array 0))
  ([array i]
     (prim-seq array i)))

(declare Vector)
(extend-type Array
  ISequential

  IHash
  (-hash [o] (scm-equal?-hash o))

  IEquiv
  (-equiv [a o] (equiv-sequential a o))

  IEmptyableCollection
  (-empty [coll] (array))
    
  IWithMeta
  (-with-meta [coll meta] (Vector. meta coll))

  ISeqable
  (-seq [array] (array-seq array 0))

  IPairable
  (-pair [coll] (scm* [coll] (vector->list coll)))

  ICounted
  (-count [a] (alength a))

  ICollection
  (-conj [coll o]
    (scm* [coll o] (vector-append coll (vector o))))

  IAssociative
  (-assoc [array k v]
    (let [new-array (aclone array)]
      (aset new-array k v)
      new-array))
  
  IVector
  (-assoc-n [coll n val] (-assoc coll n val))

   IIndexed
  (-nth
    ([array n]
       (if (< n (alength array)) (aget array n)))
    ([array n not-found]
       (if (< n (alength array)) (aget array n)
           not-found)))

  ILookup
  (-lookup
    ([array k]
       (aget array k))
    ([array k not-found]
       (-nth array k not-found)))

  IFn
  (-invoke [coll [k not-found]]
    (-lookup coll k not-found))

  IReduce
  (-reduce
    ([array f]
       (ci-reduce array f))
    ([array f start]
       (ci-reduce array f start))))

(declare with-meta)

(deftype RSeq [ci i meta]
  IMeta
  (-meta [coll] meta)
  IWithMeta
  (-with-meta [coll new-meta]
    (RSeq. ci i new-meta))

  ISeqable
  (-seq [coll] coll)

  ISequential
  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  ISeq
  (-first [coll]
    (-nth ci i))
  (-rest [coll]
    (if (pos? i)
      (RSeq. ci (dec i) nil)
      ()))

  ICounted
  (-count [coll] (inc i))

  ICollection
  (-conj [coll o]
    (cons o coll))

  IEmptyableCollection
  (-empty [coll] (with-meta List-EMPTY meta))

  IHash
  (-hash [coll] (hash-coll coll)))

(defn second
  "Same as (first (next x))"
  [coll]
  (first (next coll)))

(defn ffirst
  "Same as (first (first x))"
  [coll]
  (first (first coll)))

(defn nfirst
  "Same as (next (first x))"
  [coll]
  (next (first coll)))

(defn fnext
  "Same as (first (next x))"
  [coll]
  (first (next coll)))

(defn nnext
  "Same as (next (next x))"
  [coll]
  (next (next coll)))

(defn last
  "Return the last item in coll, in linear time"
  [s]
  (let [sn (next s)]
    (if-not (nil? sn)
      (recur sn)
      (first s))))

#_(extend-type default
  IEquiv
  (-equiv [x o] (identical? x o)))

(defn conj
  "conj[oin]. Returns a new collection with the xs
  'added'. (conj nil item) returns (item).  The 'addition' may
  happen at different 'places' depending on the concrete type."
  ([coll x]
     (-conj coll x))
  ([coll x & xs]
     (if xs
       (recur (conj coll x) (first xs) (next xs))
       (conj coll x))))

(defn empty
  "Returns an empty collection of the same category as coll, or nil"
  [coll]
  (-empty coll))

(defn- accumulating-seq-count [coll]
  (loop [s (seq coll) acc 0]
    (if (counted? s) ; assumes nil is counted, which it currently is
      (+ acc (-count s))
      (recur (next s) (inc acc)))))

(defn count
  "Returns the number of items in the collection. (count nil) returns
  0.  Also works on strings, arrays, and Maps"
  [coll]
  (if (counted? coll)
    (-count coll)
    (accumulating-seq-count coll)))

(defn- linear-traversal-nth
  ([coll n]
     (cond
       (nil? coll)     (throw (js/Error. "Index out of bounds"))
       (zero? n)       (if (seq coll)
                         (first coll)
                         (throw (js/Error. "Index out of bounds")))
       (indexed? coll) (-nth coll n)
       (seq coll)      (recur (next coll) (dec n))
       :else           (throw (js/Error. "Index out of bounds"))))
  ([coll n not-found]
     (cond
       (nil? coll)     not-found
       (zero? n)       (if (seq coll)
                         (first coll)
                         not-found)
       (indexed? coll) (-nth coll n not-found)
       (seq coll)      (recur (next coll) (dec n) not-found)
       :else           not-found)))

(defn nth
  "Returns the value at the index. get returns nil if index out of
  bounds, nth throws an exception unless not-found is supplied.  nth
  also works for strings, arrays, regex Matchers and Lists, and,
  in O(n) time, for sequences."
  ([coll n]
     (when-not (nil? coll)
       (if (satisfies? IIndexed coll)
         (-nth coll n)
         (linear-traversal-nth coll n))))
  ([coll n not-found]
     (if-not (nil? coll)
       (if (satisfies? IIndexed coll)
         (-nth coll n not-found)
         (linear-traversal-nth coll n not-found))
       not-found)))

(defn get
  "Returns the value mapped to key, not-found or nil if key not present."
  ([o k]
     (-lookup o k))
  ([o k not-found]
     (-lookup o k not-found)))

(defn assoc
  "assoc[iate]. When applied to a map, returns a new map of the
   same (hashed/sorted) type, that contains the mapping of key(s) to
   val(s). When applied to a vector, returns a new vector that
   contains val at index."
  ([coll k v]
     (-assoc coll k v))
  ([coll k v kvs]
     (let [ret (-assoc coll k v)]
       (if kvs
         (recur ret (first kvs) (second kvs) (nnext kvs))
         ret)))
  ([coll k v k2 v2 & kvs]
     (assoc coll k v (concat [k2 v2] kvs))))

(defn dissoc
  "dissoc[iate]. Returns a new map of the same (hashed/sorted) type,
  that does not contain a mapping for key(s)."
  ([coll] coll)
  ([coll k]
     (-dissoc coll k))
  ([coll k & ks]
     (let [ret (dissoc coll k)]
       (if ks
         (apply dissoc ret (first ks) (next ks))
         ret))))

(defn with-meta
  "Returns an object of the same type and value as obj, with
  map m as its metadata."
  [o meta]
  (-with-meta o meta))

(defn meta
  "Returns the metadata of obj, returns nil if there is no metadata."
  [o]
  (when (satisfies? IMeta o)
    (-meta o)))

(defn peek
  "For a list or queue, same as first, for a vector, same as, but much
  more efficient than, last. If the collection is empty, returns nil."
  [coll]
  (-peek coll))

(defn pop
  "For a list or queue, returns a new list/queue without the first
  item, for a vector, returns a new vector without the last item.
  Note - not the same as next/butlast."
  [coll]
  (-pop coll))

(defn disj
  "disj[oin]. Returns a new set of the same (hashed/sorted) type, that
  does not contain key(s)."
  ([coll] coll)
  ([coll k]
     (-disjoin coll k))
  ([coll k & ks]
     (let [ret (disj coll k)]
       (if ks
         (apply disj ret (first ks) (next ks))
         ret))))

;; Simple caching of string hashcode
#_(def string-hash-cache (js-obj))
#_(def string-hash-cache-count 0)

#_(defn add-to-string-hash-cache [k]
  (let [h (goog.string/hashCode k)]
    (aset string-hash-cache k h)
    (set! string-hash-cache-count (inc string-hash-cache-count))
    h))

#_(defn check-string-hash-cache [k]
  (when (> string-hash-cache-count 255)
    (set! string-hash-cache (js-obj))
    (set! string-hash-cache-count 0))
  (let [h (aget string-hash-cache k)]
    (if-not (nil? h)
      h
      (add-to-string-hash-cache k))))

(defn hash
  ([o] (hash o true))
  ([o ^boolean check-cache]
     (-hash o)
     #_(if (and ^boolean (goog/isString o) check-cache)
       (check-string-hash-cache o)
       (-hash o))))

(defn ^boolean empty?
  "Returns true if coll has no items - same as (not (seq coll)).
  Please use the idiom (seq x) rather than (not (empty? x))"
  [coll] (or (nil? coll)
             (not (seq coll))))

(defn ^boolean coll?
  "Returns true if x satisfies ICollection"
  [x]
  (if (nil? x)
    false
    (satisfies? ICollection x)))

(defn ^boolean set?
  "Returns true if x satisfies ISet"
  [x]
  (if (nil? x)
    false
    (satisfies? ISet x)))

(defn ^boolean associative?
 "Returns true if coll implements Associative"
  [x] (satisfies? IAssociative x))

(defn ^boolean sequential?
  "Returns true if coll satisfies ISequential"
  [x] (satisfies? ISequential x))

(defn ^boolean reduceable?
  "Returns true if coll satisfies IReduce"
  [x] (satisfies? IReduce x))

(defn ^boolean map?
  "Return true if x satisfies IMap"
  [x]
  (if (nil? x)
    false
    (satisfies? IMap x)))

(defn ^boolean vector?
  "Return true if x satisfies IVector"
  [x] (satisfies? IVector x))

(defn ^boolean chunked-seq?
  [x] (satisfies? IChunkedSeq x))

;;;;;;;;;;;;;;;;;;;; js primitives ;;;;;;;;;;;;
(defn js-obj
  ([]
     (scm* {} (make-table)))
  #_([& keyvals]
     (apply gobject/create keyvals)))

#_(defn js-keys [obj]
  (let [keys (array)]
    (goog.object/forEach obj (fn [val key obj] (.push keys key)))
    keys))

#_(defn js-delete [obj key]
  (js* "delete ~{obj}[~{key}]"))

(defn- array-copy
  ([from i to j len]
     (loop [i i j j len len]
       (if (zero? len)
         to
         (do (aset to j (aget from i))
             (recur (inc i) (inc j) (dec len)))))))

(defn- array-copy-downward
  ([from i to j len]
     (loop [i (+ i (dec len)) j (+ j (dec len)) len len]
       (if (zero? len)
         to
         (do (aset to j (aget from i))
             (recur (dec i) (dec j) (dec len)))))))

;;;;;;;;;;;;;;;; preds ;;;;;;;;;;;;;;;;;;

(def ^:private lookup-sentinel (fn []))

(defn ^boolean false?
  "Returns true if x is the value false, false otherwise."
  [x] (identical? x false))

(defn ^boolean true?
  "Returns true if x is the value true, false otherwise."
  [x] (identical? x true))

(defn ^boolean undefined? [x]
  (nil? x))

(defn ^boolean seq?
  "Return true if s satisfies ISeq"
  [s]
  (if (nil? s)
    false
    (satisfies? ISeq s)))

(defn ^boolean seqable?
  "Return true if s satisfies ISeqable"
  [s]
  (satisfies? ISeqable s))

(defn ^boolean boolean [x]
  (if x true false))

(defn ^boolean string? [x]
  (instance? String x))

(defn ^boolean keyword? [x]
  (instance? Keyword x))

(defn ^boolean symbol? [x]
  (or (instance? Symbol x)
      (instance? Symbol+ x)))

(defn ^boolean number? [n]
  (instance? Number n))

(defn ^boolean fn? [f]
  (satisfies? Fn f))

(defn ^boolean ifn? [f]
  (or (fn? f) (satisfies? IFn f)))

(defn ^boolean integer?
  "Returns true if n is an integer."
  [n]
  (scm* [n] (integer? n)))

(defn ^boolean contains?
  "Returns true if key is present in the given collection, otherwise
  returns false.  Note that for numerically indexed collections like
  vectors and arrays, this tests if the numeric key is within the
  range of indexes. 'contains?' operates constant or logarithmic time;
  it will not perform a linear search for a value.  See also 'some'."
  [coll v]
  (if (identical? (-lookup coll v lookup-sentinel) lookup-sentinel)
    false
    true))

(defn find
  "Returns the map entry for key, or nil if key not present."
  [coll k]
  (when (and (not (nil? coll))
             (associative? coll)
             (contains? coll k))
    [k (-lookup coll k)]))

(defn ^boolean distinct?
  "Returns true if no two of the arguments are ="
  ([x] true)
  ([x y] (not (= x y)))
  ([x y & more]
     (if (not (= x y))
     (loop [s #{x y} xs more]
       (let [x (first xs)
             etc (next xs)]
         (if xs
           (if (contains? s x)
             false
             (recur (conj s x) etc))
           true)))
     false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Seq fns ;;;;;;;;;;;;;;;;

(defn compare
  "Comparator. Returns a negative number, zero, or a positive number
  when x is logically 'less than', 'equal to', or 'greater than'
  y. Uses google.array.defaultCompare."
  [x y]
  (cond
   (identical? x y) 0
   (nil? x) -1
   (nil? y) 1
   (identical? Number (type x)) (< x y)
   (identical? Char (type x)) (scm* [x y] (char<? x y))
   (identical? String (type x)) (scm* [x y] (string<? x y))
   (and (identical? (type x) (type y))
        (satisfies? IComparable x)) (-compare x y)
        :else (throw (Error. "compare on non-nil objects of different types"))))

(defn comparator
  [f]
  (fn [x y] (if (f x y) -1 +1)))

(defn ^:private compare-indexed
  "Compare indexed collection."
  ([xs ys]
     (let [xl (count xs)
           yl (count ys)]
       (cond
        (< xl yl) -1
        (> xl yl) 1
        :else (compare-indexed xs ys xl 0))))
  ([xs ys len n]
     (let [d (compare (nth xs n) (nth ys n))]
       (if (and (zero? d) (< (+ n 1) len))
         (recur xs ys len (inc n))
         d))))

(defn ^:private fn->comparator
  "Given a fn that might be boolean valued or a comparator,
   return a fn that is a comparator."
  [f]
  (if (= f compare)
    compare
    (fn [x y]
      (let [r (f x y)]
        (if (number? r)
          r
          (if r
            -1
            (if (f y x) 1 0)))))))

(declare to-array)

;;; File: "Sort.scm", Time-stamp: <2008-03-18 15:21:35 feeley>
;;; Copyright (c) 2006-2008 by Marc Feeley, All Rights Reserved.
(scm* []
      (define (sort sequence less?)

        (declare (standard-bindings) (not safe))

        (define (sort-list lst less?)

          (define (mergesort lst)

            (define (merge lst1 lst2)
              (cond ((not (pair? lst1))
                     lst2)
                    ((not (pair? lst2))
                     lst1)
                    (else
                     (let ((e1 (car lst1)) (e2 (car lst2)))
                       (if (less? e1 e2)
                         (cons e1 (merge (cdr lst1) lst2))
                         (cons e2 (merge lst1 (cdr lst2))))))))

            (define (split lst)
              (if (or (not (pair? lst)) (not (pair? (cdr lst))))
                lst
                (cons (car lst) (split (cddr lst)))))

            (if (or (not (pair? lst)) (not (pair? (cdr lst))))
              lst
              (let* ((lst1 (mergesort (split lst)))
                     (lst2 (mergesort (split (cdr lst)))))
                    (merge lst1 lst2))))

          (mergesort lst))

        (cond ((not (procedure? less?))
               (error "procedure expected"))
              ((or (null? sequence)
                   (pair? sequence))
               (sort-list sequence less?))
              ((vector? sequence)
               (list->vector (sort-list (vector->list sequence) less?)))
              (else
               (error "vector or list expected")))))

(defn sort
  "Returns a sorted sequence of the items in coll. Comp can be
   boolean-valued comparison funcion, or a -/0/+ valued comparator.
   Comp defaults to compare."
  ([coll]
   (sort compare coll))
  ([comp coll]
   (if (seq coll)
     (let [a (to-array coll)]
       ;; matching Clojure's stable sort, though docs don't promise it
       (seq (scm* [a comp] (sort a comp))))
     ())))

(defn sort-by
  "Returns a sorted sequence of the items in coll, where the sort
   order is determined by comparing (keyfn item).  Comp can be
   boolean-valued comparison funcion, or a -/0/+ valued comparator.
   Comp defaults to compare."
  ([keyfn coll]
   (sort-by keyfn compare coll))
  ([keyfn comp coll]
     (sort (fn [x y] (comp (keyfn x) (keyfn y))) coll)))

; simple reduce based on seqs, used as default
(defn- seq-reduce
  ([f coll]
    (if-let [s (seq coll)]
      (reduce f (first s) (next s))
      (f)))
  ([f val coll]
    (loop [val val, coll (seq coll)]
      (if coll
        (let [nval (f val (first coll))]
          (if (reduced? nval)
            @nval
            (recur nval (next coll))))
        val))))

; simple nth based on seqs, used as default
(defn- seq-nth
  ([coll n]
     (if (= 0 n)
       (first coll)
       (if-let [nxt (next coll)]
         (seq-nth nxt (dec n))
         (throw "Index out of range"))))
  ([coll n not-found]
     (if (= 0 n)
       (first coll)
       (if-let [nxt (next coll)]
         (seq-nth nxt (dec n) not-found)
         not-found))))

(declare vec)

(defn shuffle
  "Return a random permutation of coll"
  [coll]
  (let [a (to-array coll)]
    (garray/shuffle a)
    (vec a)))

(defn reduce
  "f should be a function of 2 arguments. If val is not supplied,
  returns the result of applying f to the first 2 items in coll, then
  applying f to that result and the 3rd item, etc. If coll contains no
  items, f must accept no arguments as well, and reduce returns the
  result of calling f with no arguments.  If coll has only 1 item, it
  is returned and f is not called.  If val is supplied, returns the
  result of applying f to val and the first item in coll, then
  applying f to that result and the 2nd item, etc. If coll contains no
  items, returns val and f is not called."
  ([f coll]
     (if (satisfies? IReduce coll)
       (-reduce coll f)
       (seq-reduce f coll)))
  ([f val coll]
     (if (satisfies? IReduce coll)
       (-reduce coll f val)
       (seq-reduce f val coll))))

(defn reduce-kv
  "Reduces an associative collection. f should be a function of 3
  arguments. Returns the result of applying f to init, the first key
  and the first value in coll, then applying f to that result and the
  2nd key and value, etc. If coll contains no entries, returns init
  and f is not called. Note that reduce-kv is supported on vectors,
  where the keys will be the ordinals."
  ([f init coll]
     (-kv-reduce coll f init)))

;;; Math - variadic forms will not work until the following implemented:
;;; first, next, reduce

(def +
  "Returns the sum of nums. (+) returns 0."
  (scm* [] +))

(def - 
  "If no ys are supplied, returns the negation of x, else subtracts
  the ys from x and returns the result."
  (scm* [] -))

(def *
  "Returns the product of nums. (*) returns 1."
  (scm* [] *))

(def /
  "If no denominators are supplied, returns 1/numerator,
  else returns numerator divided by all of the denominators."
  (scm* [] /))

(def ^boolean <
  "Returns non-nil if nums are in monotonically increasing order,
  otherwise false."
  (scm* [] <))

(def ^boolean <=
  "Returns non-nil if nums are in monotonically non-decreasing order,
  otherwise false."
  (scm* [] <=))

(def ^boolean >
  "Returns non-nil if nums are in monotonically decreasing order,
  otherwise false."
  (scm* [] >))

(def ^boolean >=
  "Returns non-nil if nums are in monotonically non-increasing order,
  otherwise false."
  (scm* [] >=))

(defn dec
  "Returns a number one less than num."
  [x] (- x 1))

(def max
  "Returns the greatest of the nums."
  (scm* [] max))

(def min
  "Returns the least of the nums."
  (scm* [] min))

(defn- fix [q]
  (if (> q 0)
    (scm* [q] (inexact->exact (floor q)))
    (scm* [q] (inexact->exact (ceiling q)))))

(defn int
  "Coerce to int by stripping decimal places."
  [x]
  (if (char? x)
    (scm* [x] (char->integer x))
    (fix x)))

(defn long
  "Coerce to long by stripping decimal places. Identical to `int'."
  [x]
  (fix x))

#_(defn js-mod
  "Modulus of num and div with original javascript behavior. i.e. bug for negative numbers"
  [n d]
  (cljscm.core/js-mod n d))

(defn mod
  "Modulus of num and div. Truncates toward negative infinity."
  [n d]
  (scm* {::num n ::div d}
        (if (or (flonum? ::num) (flonum? ::div))
          (- ::num (* :div (fltruncate (/ ::num ::div))))
          (modulo ::num ::div)))) ;(+ (js-mod n d) d) d

(defn quot
  "quot[ient] of dividing numerator by denominator."
  [n d]
  (scm* {::num n ::div d}
        (if (or (flonum? ::num) (flonum? ::div))
          (- ::num (* :div (fltruncate (/ ::num ::div))))
          (quotient ::num ::div))))

(defn rem
  "remainder of dividing numerator by denominator."
  [n d]
  (scm* {::num n ::div d}
        (if (or (flonum? ::num) (flonum? ::div))
          (- ::num (* ::div (fltruncate (/ ::num ::div))))
          (remainder ::num ::div))))

(defn rand
  "Returns a random floating point number between 0 (inclusive) and n (default 1) (exclusive)."
  ([]  (Math/random))
  ([n] (* n (rand))))

(defn rand-int
  "Returns a random integer between 0 (inclusive) and n (exclusive)."
  [n] (fix (rand n)))

(defn bit-xor
  "Bitwise exclusive or"
  [x y] ((scm* {} bitwise-xor) x y))

(defn bit-and
  "Bitwise and"
  [x y] ((scm* {} bitwise-and) x y))

(defn bit-or
  "Bitwise or"
  [x y] ((scm* {} bitwise-ior) x y))

(defn bit-not
  "Bitwise complement"
  [x] ((scm* {} bitwise-not) x))

(defn bit-and-not
  "Bitwise and"
  [x y] (bit-and x (bit-not y)))

(comment
(defn bit-clear
  "Clear bit at index n"
  [x n]
  (cljscm.core/bit-clear x n))

(defn bit-flip
  "Flip bit at index n"
  [x n]
  (cljscm.core/bit-flip x n))

(defn bit-set
  "Set bit at index n"
  [x n]
  (cljscm.core/bit-set x n))

(defn bit-test
  "Test bit at index n"
  [x n]
  (cljscm.core/bit-test x n)))

(defn bit-shift-left
  "Bitwise shift left, 32 bit"
  [x n]
  (bit-and ((scm* {} arithmetic-shift) x n) 4294967295))

(defn bit-shift-right
  "Bitwise shift right"
  [x n]
  ((scm* {} arithmetic-shift) x (* -1 n)))

(defn bit-shift-right-zero-fill
  "Bitwise shift right with zero fill"
  [x n] ((scm* {} arithmetic-shift) (bit-and x 4294967295) (* -1 n)) )

(defn bit-count
  "Counts the number of bits set in n"
  [v]
  ((scm* {} bit-count) v)
  #_(let [v (- v (bit-and (bit-shift-right v 1) 0x55555555))
        v (+ (bit-and v 0x33333333) (bit-and (bit-shift-right v 2) 0x33333333))]
    (bit-shift-right (* (bit-and (+ v (bit-shift-right v 4)) 0xF0F0F0F) 0x1010101) 24)))

(defn ^boolean ==
  "Returns non-nil if nums all have the equivalent
  value, otherwise false. Behavior on non nums is
  undefined."
  ([x] true)
  ([x y] (-equiv x y))
  ([x y & more]
   (if (== x y)
     (if (next more)
       (recur y (first more) (next more))
       (== y (first more)))
     false)))

(defn ^boolean pos?
  "Returns true if num is greater than zero, else false"
  [n] ((scm* {} positive?) n))

(defn ^boolean zero? [n]
  (identical? n 0))

(defn ^boolean neg?
  "Returns true if num is less than zero, else false"
  [x] ((scm* {} negative?) x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; protocols for host types ;;;;;;

(defn nthnext
  "Returns the nth next of coll, (seq coll) when n is 0."
  [coll n]
  (loop [n n xs (seq coll)]
    (if (and xs (pos? n))
      (recur (dec n) (next xs))
      xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;; basics ;;;;;;;;;;;;;;;;;;

(defn str
  "With no args, returns the empty string. With one arg x, returns
  x.toString().  (str nil) returns the empty string. With more than
  one arg, returns the concatenation of the str values of the args."
  ([] "")
  ([x] (if (satisfies? IStringable x)
         (-toString x)
         (pr-str x)))
  ([x & ys]
     (scm* {:strs (pair (map str (cons x ys)))}
           (append-strings :strs))))

(defn subs
  "Returns the substring of s beginning at start inclusive, and ending
  at end (defaults to length of string), exclusive."
  ([s start] (scm* [s start] (substring s start (string-length s))))
  ([s start end] (scm* [s start end] (substring s start end))))

(declare map)

(defn format
  "Formats a string using goog.string.format."
  [fmt & args]
  (let [args (map (fn [x]
                    (if (or (keyword? x) (symbol? x))
                      (str x)
                      x))
                args)]
    (apply gstring/format fmt args)))

(defn symbol
  "Returns a Symbol with the given namespace and name."
  ([name] (cond
         (symbol? name) name
         (keyword? name) (scm* [name] (string->symbol (keyword->string name)))
         (string? name) (scm* [name] (string->symbol name))))
  ([ns name] (if ns
               (symbol (str ns "/" name))
               (symbol name))))

(defn keyword
  "Returns a Keyword with the given namespace and name.  Do not use :
  in the keyword strings, it will be added automatically."
  ([name] (cond (keyword? name) name
                (symbol? name) (keyword (str name))
                :else (scm* [name] (string->keyword name))))
  ([ns name] (keyword (str ns "/" name))))



(defn- equiv-sequential
  "Assumes x is sequential. Returns true if x equals y, otherwise
  returns false."
  [x y]
  (boolean
   (when (sequential? y)
     (loop [xs (seq x) ys (seq y)]
       (cond (nil? xs) (nil? ys)
             (nil? ys) false
             (= (first xs) (first ys)) (recur (next xs) (next ys))
             :else false)))))

(defn hash-combine [seed hash]
  ; a la boost
  (bit-xor seed (+ hash 0x9e3779b9
                   (bit-shift-left seed 6)
                   (bit-shift-right seed 2))))

(defn- hash-coll [coll]
  (reduce #(hash-combine %1 (hash %2 false)) (hash (first coll) false) (next coll)))

(declare key val)

(defn- hash-imap [m]
  ;; a la clojure.lang.APersistentMap
  (loop [h 0 s (seq m)]
    (if s
      (let [e (first s)]
        (recur (mod (+ h (bit-xor (hash (key e)) (hash (val e))))
                    4503599627370496)
               (next s)))
      h)))

(defn- hash-iset [s]
  ;; a la clojure.lang.APersistentSet
  (loop [h 0 s (seq s)]
    (if s
      (let [e (first s)]
        (recur (mod (+ h (hash e)) 4503599627370496)
               (next s)))
      h)))

(declare name)

(defn- extend-object!
  "Takes a JavaScript object and a map of names to functions and
  attaches said functions as methods on the object.  Any references to
  JavaScript's implict this (via the this-as macro) will resolve to the
  object that the function is attached."
  [obj fn-map]
  (doseq [[key-name f] fn-map]
    (let [str-name (name key-name)]
      (aset obj str-name f)))
  obj)

;;;;;;;;;;;;;;;; cons ;;;;;;;;;;;;;;;;

(declare empty-list)

(deftype EmptyList [meta]
  IList

  IWithMeta
  (-with-meta [coll meta] (EmptyList. meta))

  IMeta
  (-meta [coll] meta)

  ISeq
  (-first [coll] nil)
  (-rest [coll] ())

  INext
  (-next [coll] nil)

  IStack
  (-peek [coll] nil)

  (-pop [coll] (throw (Error. "Can't pop empty list")))

  ICollection
  (-conj [coll o] (Cons. meta o empty-list nil))

  IEmptyableCollection
  (-empty [coll] coll)

  ISequential
  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  IHash
  (-hash [coll] 0)

  ISeqable
  (-seq [coll] nil)
  IPairable
  (-pair [coll] (scm* [] '()))

  ICounted
  (-count [coll] 0)

  IIndexed
  (-nth
    ([coll n]
       (throw (str "index " n " out of bounds.")))
    ([coll n not-found]
       not-found))
  
  IReduce
  (-reduce
    ([_ f] (f))
    ([_ f start] start)))

(def empty-list (EmptyList. nil))



(defn ^boolean reversible? [coll]
  (satisfies? IReversible coll))

(defn rseq [coll]
  (-rseq coll))

(defn reverse
  "Returns a seq of the items in coll in reverse order. Not lazy."
  [coll]
  (if (reversible? coll)
    (rseq coll)
    (reduce conj () coll)))

(defn list
  ([] ())
  ([x] (conj () x))
  ([x y] (conj (list y) x))
  ([x y z] (conj (list y z) x))
  ([x y z & items]
     (conj (conj (conj (reduce conj () (reverse items))
                       z) y) x)))

(deftype Cons [meta first rest ^:mutable __hash]
  IList

  IWithMeta
  (-with-meta [coll meta] (Cons. meta first rest __hash))

  IMeta
  (-meta [coll] meta)

  ASeq
  ISeq
  (-first [coll] first)
  (-rest [coll] (if (nil? rest) () rest))

  IStack
  (-peek [coll] first)
  (-pop [coll] (-rest coll))

  INext
  (-next [coll] (if (nil? rest) nil (-seq rest)))

  ICollection
  (-conj [coll o] (Cons. nil o coll __hash))

  IEmptyableCollection
  (-empty [coll] (with-meta empty-list meta))

  ISequential
  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  IHash
  (-hash [coll] (caching-hash coll hash-coll __hash))

  ISeqable
  (-seq [coll] coll)

  ICounted
  (-count [c] (inc (count rest)))

  IIndexed
  (-nth
    ([coll n] (seq-nth coll n))
    ([coll n not-found] (seq-nth coll n not-found)))

  IReduce 
  (-reduce
    ([v f] (seq-reduce f v))
    ([v f start] (seq-reduce f start v)))

  IPairable
  (-pair [coll] (let [pairedrest (pair rest)]
                  (scm* [first pairedrest] (cons first pairedrest)))))

(defn cons
  "Returns a new seq where x is the first element and seq is the rest."
  [x coll]
  (if (or (nil? coll)
          (satisfies? ISeq coll))
    (Cons. nil x coll nil)
    (Cons. nil x (seq coll) nil)))

(defn ^boolean list? [x]
  (satisfies? IList x))

#_(set! js/String.prototype.apply
      (fn
        [s args]
        (if (< (count args) 2)
          (get (aget args 0) s)
          (get (aget args 0) s (aget args 1)))))

(extend-type String
  IHash
  (-hash [o] (scm-equal?-hash o))

  IEquiv
  (-equiv [s o] (scm* [s o] (equal? s o)))

  ISeqable
  (-seq [string] (scm* [string] (string->list string)))

  ICounted
  (-count [s] (scm* [s] (string-length s)))

  IIndexed
  (-nth
    ([string n] ;//TODO could catch exception
       (if (< n (-count string)) (scm* [string n] (string-ref string n))))
    ([string n not-found]
       (if (< n (-count string)) (scm* [string n] (string-ref string n))
           not-found)))

  ILookup
  (-lookup
    ([string k]
       (-nth string k))
    ([string k not_found]
       (-nth string k not_found)))
  IReduce
  (-reduce
    ([string f]
       (ci-reduce string f))
    ([string f start]
       (ci-reduce string f start)))

  IStringable
  (-toString [o] o))

; could use reify
;;; LazySeq ;;;

(defn- lazy-seq-value [lazy-seq]
  (let [x (scm* [lazy-seq] (cljscm.core/LazySeq-x lazy-seq))]
    (if ^boolean (scm* [lazy-seq] (cljscm.core/LazySeq-realized lazy-seq))
      x
      (do
        (let [forced-val (x)]
          (scm* [lazy-seq forced-val] (cljscm.core/LazySeq-x-set! lazy-seq forced-val))
          (scm* [lazy-seq true] (cljscm.core/LazySeq-realized-set! lazy-seq true))
          forced-val)))))

(deftype LazySeq [meta realized x ^:mutable __hash]
  IWithMeta
  (-with-meta [coll meta] (LazySeq. meta realized x __hash))

  IMeta
  (-meta [coll] meta)

  ISeq
  (-first [coll] (first (lazy-seq-value coll)))
  (-rest [coll] (rest (lazy-seq-value coll)))

  INext
  (-next [coll] (-seq (-rest coll)))

  ICollection
  (-conj [coll o] (cons o coll))

  IEmptyableCollection
  (-empty [coll] (with-meta empty-list meta))

  ISequential
  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  IHash
  (-hash [coll] (caching-hash coll hash-coll __hash))

  ISeqable
  (-seq [coll] (seq (lazy-seq-value coll)))

  IIndexed
  (-nth
    ([coll n] (seq-nth coll n))
    ([coll n not-found] (seq-nth coll n not-found)))

  IReduce 
  (-reduce
    ([v f] (seq-reduce f v))
    ([v f start] (seq-reduce f start v))))

(declare ArrayChunk)

(deftype ChunkBuffer [^:mutable buf ^:mutable end]
  ICounted
  (-count [_] end))

(defn chunk-buffer [capacity]
  (ChunkBuffer. (make-array capacity) 0))

(deftype ArrayChunk [arr off end]
  ICounted
  (-count [_] (- end off))

  IIndexed
  (-nth [coll i]
    (aget arr (+ off i)))
  (-nth [coll i not-found]
    (if (and (>= i 0) (< i (- end off)))
      (aget arr (+ off i))
      not-found))

  IChunk
  (-drop-first [coll]
    (if (== off end)
      (throw (js/Error. "-drop-first of empty chunk"))
      (ArrayChunk. arr (inc off) end)))

  IReduce
  (-reduce [coll f]
    (array-reduce arr f (aget arr off) (inc off)))
  (-reduce [coll f start]
    (array-reduce arr f start off)))

(defn array-chunk
  ([arr]
     (array-chunk arr 0 (alength arr)))
  ([arr off]
     (array-chunk arr off (alength arr)))
  ([arr off end]
     (ArrayChunk. arr off end)))

(deftype ChunkedCons [chunk more meta ^:mutable __hash]
  IWithMeta
  (-with-meta [coll m]
    (ChunkedCons. chunk more m __hash))

  IMeta
  (-meta [coll] meta)

  ISequential
  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  ISeqable
  (-seq [coll] coll)

  ASeq
  ISeq
  (-first [coll] (-nth chunk 0))
  (-rest [coll]
    (if (> (-count chunk) 1)
      (ChunkedCons. (-drop-first chunk) more meta nil)
      (if (nil? more)
        ()
        more)))

  IChunkedSeq
  (-chunked-first [coll] chunk)
  (-chunked-rest [coll]
    (if (nil? more)
      ()
      more))

  IChunkedNext
  (-chunked-next [coll]
    (if (nil? more)
      nil
      more))

  ICollection
  (-conj [this o]
    (cons o this))

  IEmptyableCollection
  (-empty [coll] (with-meta List-EMPTY meta))

  IHash
  (-hash [coll] (caching-hash coll hash-coll __hash)))

(defn chunk-cons [chunk rest]
  (if (zero? (-count chunk))
    rest
    (ChunkedCons. chunk rest nil nil)))

(defn chunk-append [b x]
  (aset (.-buf b) (.-end b) x)
  (set! (.-end b) (inc (.-end b))))

(defn chunk [b]
  (let [ret (ArrayChunk. (.-buf b) 0 (.-end b))]
      (set! (.-buf b) nil)
      ret))

(defn chunk-first [s]
  (-chunked-first s))

(defn chunk-rest [s]
  (-chunked-rest s))

(defn chunk-next [s]
  (if (satisfies? IChunkedNext s)
    (-chunked-next s)
    (seq (-chunked-rest s))))

;;;;;;;;;;;;;;;;

(defn to-array
  [s]
  (apply array s))

(defn to-array-2d
  "Returns a (potentially-ragged) 2-dimensional array
  containing the contents of coll."
  [coll]
    (let [ret (make-array (count coll))]
      (loop [i 0 xs (seq coll)]
        (when xs
          (aset ret i (to-array (first xs)))
          (recur (inc i) (next xs))))
      ret))

(defn long-array
  ([size-or-seq]
     (cond
      (number? size-or-seq) (long-array size-or-seq nil)
      (seq? size-or-seq) (into-array size-or-seq)
      :else (throw (js/Error. "long-array called with something other than size or ISeq"))))
  ([size init-val-or-seq]
     (let [a (make-array size)]
       (if (seq? init-val-or-seq)
         (let [s (seq init-val-or-seq)]
           (loop [i 0 s s]
             (if (and s (< i size))
               (do
                 (aset a i (first s))
                 (recur (inc i) (next s)))
               a)))
         (do
           (dotimes [i size]
             (aset a i init-val-or-seq))
           a)))))

(defn double-array
  ([size-or-seq]
     (cond
      (number? size-or-seq) (double-array size-or-seq nil)
      (seq? size-or-seq) (into-array size-or-seq)
      :else (throw (js/Error. "double-array called with something other than size or ISeq"))))
  ([size init-val-or-seq]
     (let [a (make-array size)]
       (if (seq? init-val-or-seq)
         (let [s (seq init-val-or-seq)]
           (loop [i 0 s s]
             (if (and s (< i size))
               (do
                 (aset a i (first s))
                 (recur (inc i) (next s)))
               a)))
         (do
           (dotimes [i size]
             (aset a i init-val-or-seq))
           a)))))

(defn object-array
  ([size-or-seq]
     (cond
      (number? size-or-seq) (object-array size-or-seq nil)
      (seq? size-or-seq) (into-array size-or-seq)
      :else (throw (js/Error. "object-array called with something other than size or ISeq"))))
  ([size init-val-or-seq]
     (let [a (make-array size)]
       (if (seq? init-val-or-seq)
         (let [s (seq init-val-or-seq)]
           (loop [i 0 s s]
             (if (and s (< i size))
               (do
                 (aset a i (first s))
                 (recur (inc i) (next s)))
               a)))
         (do
           (dotimes [i size]
             (aset a i init-val-or-seq))
           a)))))

(defn- bounded-count [s n]
  (if (counted? s)
    (count s)
    (loop [s s i n sum 0]
      (if (and (pos? i) (seq s))
        (recur (next s) (dec i) (inc sum))
        sum))))

(defn spread
  [arglist]
  (cond
   (nil? arglist) nil
   (nil? (next arglist)) (seq (first arglist))
   :else (cons (first arglist)
               (spread (next arglist)))))

(defn concat
  "Returns a lazy seq representing the concatenation of the elements in the supplied colls."
  ([] (lazy-seq nil))
  ([x] (lazy-seq x))
  ([x y]
    (lazy-seq
      (let [s (seq x)]
        (if s
          (if (chunked-seq? s)
            (chunk-cons (chunk-first s) (concat (chunk-rest s) y))
            (cons (first s) (concat (rest s) y)))
          y))))
  ([x y & zs]
     (let [cat (fn cat [xys zs]
                 (lazy-seq
                   (let [xys (seq xys)]
                     (if xys
                       (if (chunked-seq? xys)
                         (chunk-cons (chunk-first xys)
                                     (cat (chunk-rest xys) zs))
                         (cons (first xys) (cat (rest xys) zs)))
                       (when zs
                         (cat (first zs) (next zs)))))))]
       (cat (concat x y) zs))))

(defn list*
  "Creates a new list containing the items prepended to the rest, the
  last of which will be treated as a sequence."
  ([args] (seq args))
  ([a args] (cons a args))
  ([a b args] (cons a (cons b args)))
  ([a b c args] (cons a (cons b (cons c args))))
  ([a b c d & more]
     (cons a (cons b (cons c (cons d (spread more)))))))


;;; Transients

(defn transient [coll]
  (-as-transient coll))

(defn persistent! [tcoll]
  (-persistent! tcoll))

(defn conj! [tcoll val]
  (-conj! tcoll val))

(defn assoc! [tcoll key val]
  (-assoc! tcoll key val))

(defn dissoc! [tcoll key]
  (-dissoc! tcoll key))

(defn pop! [tcoll]
  (-pop! tcoll))

(defn disj! [tcoll val]
  (-disjoin! tcoll val))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; apply ;;;;;;;;;;;;;;;;

;; see core.clj
#_(gen-apply-to)

;(set! *unchecked-if* true)
(defn apply
  "Applies fn f to the argument list formed by prepending intervening arguments to args.
  First cut.  Not lazy.  Needs to use emitted toApply."
  ([f args] (let [arglist (pair args)] (scm* [f arglist] (polymorphic-apply-with-procedure-check f arglist))))
  ([f x & args] (let [r (concat [x] (butlast args) (last args))]
                  (apply f r))))

#_(defn apply
  ([f args]
     (let [fixed-arity (.-cljs$lang$maxFixedArity f)]
       (if (.-cljs$lang$applyTo f)
         (let [bc (bounded-count args (inc fixed-arity))]
          (if (<= bc fixed-arity)
            (apply-to f bc args)
            (.cljs$lang$applyTo f args)))
         (.apply f f (to-array args)))))
  ([f x args]
     (let [arglist (list* x args)
           fixed-arity (.-cljs$lang$maxFixedArity f)]
       (if (.-cljs$lang$applyTo f)
         (let [bc (bounded-count arglist (inc fixed-arity))]
          (if (<= bc fixed-arity)
            (apply-to f bc arglist)
            (.cljs$lang$applyTo f arglist)))
         (.apply f f (to-array arglist)))))
  ([f x y args]
     (let [arglist (list* x y args)
           fixed-arity (.-cljs$lang$maxFixedArity f)]
       (if (.-cljs$lang$applyTo f)
         (let [bc (bounded-count arglist (inc fixed-arity))]
          (if (<= bc fixed-arity)
            (apply-to f bc arglist)
            (.cljs$lang$applyTo f arglist)))
         (.apply f f (to-array arglist)))))
  ([f x y z args]
     (let [arglist (list* x y z args)
           fixed-arity (.-cljs$lang$maxFixedArity f)]
       (if (.-cljs$lang$applyTo f)
         (let [bc (bounded-count arglist (inc fixed-arity))]
          (if (<= bc fixed-arity)
            (apply-to f bc arglist)
            (.cljs$lang$applyTo f arglist)))
         (.apply f f (to-array arglist)))))
  ([f a b c d & args]
     (let [arglist (cons a (cons b (cons c (cons d (spread args)))))
           fixed-arity (.-cljs$lang$maxFixedArity f)]
       (if (.-cljs$lang$applyTo f)
         (let [bc (bounded-count arglist (inc fixed-arity))]
          (if (<= bc fixed-arity)
            (apply-to f bc arglist)
            (.cljs$lang$applyTo f arglist)))
         (.apply f f (to-array arglist))))))
;(set! *unchecked-if* false)

(defn vary-meta
 "Returns an object of the same type and value as obj, with
  (apply f (meta obj) args) as its metadata."
 [obj f & args]
 (with-meta obj (apply f (meta obj) args)))

(defn ^boolean not=
  "Same as (not (= obj1 obj2))"
  ([x] false)
  ([x y] (not (= x y)))
  ([x y & more]
   (not (apply = x y more))))

(defn not-empty
  "If coll is empty, returns nil, else coll"
  [coll] (when (seq coll) coll))

(defn ^boolean every?
  "Returns true if (pred x) is logical true for every x in coll, else
  false."
  [pred coll]
  (cond
   (nil? (seq coll)) true
   (pred (first coll)) (recur pred (next coll))
   :else false))

(defn ^boolean not-every?
  "Returns false if (pred x) is logical true for every x in
  coll, else true."
  [pred coll] (not (every? pred coll)))

(defn some
  "Returns the first logical true value of (pred x) for any x in coll,
  else nil.  One common idiom is to use a set as pred, for example
  this will return :fred if :fred is in the sequence, otherwise nil:
  (some #{:fred} coll)"
  [pred coll]
    (when (seq coll)
      (or (pred (first coll)) (recur pred (next coll)))))

(defn ^boolean not-any?
  "Returns false if (pred x) is logical true for any x in coll,
  else true."
  [pred coll] (not (some pred coll)))

(defn ^boolean even?
  "Returns true if n is even, throws an exception if n is not an integer"
   [n] (if (integer? n)
        (zero? (bit-and n 1))
        (throw (Error. (str "Argument must be an integer: " n)))))

(defn ^boolean odd?
  "Returns true if n is odd, throws an exception if n is not an integer"
  [n] (not (even? n)))

(defn identity [x] x)

(defn ^boolean complement
  "Takes a fn f and returns a fn that takes the same arguments as f,
  has the same effects, if any, and returns the opposite truth value."
  [f]
  (fn
    ([] (not (f)))
    ([x] (not (f x)))
    ([x y] (not (f x y)))
    ([x y & zs] (not (apply f x y zs)))))

(defn constantly
  "Returns a function that takes any number of arguments and returns x."
  [x] (fn [& args] x))

(defn comp
  "Takes a set of functions and returns a fn that is the composition
  of those fns.  The returned fn takes a variable number of args,
  applies the rightmost of fns to the args, the next
  fn (right-to-left) to the result, etc."
  ([] identity)
  ([f] f)
  ([f g]
     (fn
       ([] (f (g)))
       ([x] (f (g x)))
       ([x y] (f (g x y)))
       ([x y z] (f (g x y z)))
       ([x y z & args] (f (apply g x y z args)))))
  ([f g h]
     (fn
       ([] (f (g (h))))
       ([x] (f (g (h x))))
       ([x y] (f (g (h x y))))
       ([x y z] (f (g (h x y z))))
       ([x y z & args] (f (g (apply h x y z args))))))
  ([f1 f2 f3 & fs]
    (let [fs (reverse (list* f1 f2 f3 fs))]
      (fn [& args]
        (loop [ret (apply (first fs) args) fs (next fs)]
          (if fs
            (recur ((first fs) ret) (next fs))
            ret))))))

(defn partial
  "Takes a function f and fewer than the normal arguments to f, and
  returns a fn that takes a variable number of additional args. When
  called, the returned function calls f with args + additional args."
  ([f arg1]
   (fn [& args] (apply f arg1 args)))
  ([f arg1 arg2]
   (fn [& args] (apply f arg1 arg2 args)))
  ([f arg1 arg2 arg3]
   (fn [& args] (apply f arg1 arg2 arg3 args)))
  ([f arg1 arg2 arg3 & more]
   (fn [& args] (apply f arg1 arg2 arg3 (concat more args)))))

(defn fnil
  "Takes a function f, and returns a function that calls f, replacing
  a nil first argument to f with the supplied value x. Higher arity
  versions can replace arguments in the second and third
  positions (y, z). Note that the function f can take any number of
  arguments, not just the one(s) being nil-patched."
  ([f x]
   (fn
     ([a] (f (if (nil? a) x a)))
     ([a b] (f (if (nil? a) x a) b))
     ([a b c] (f (if (nil? a) x a) b c))
     ([a b c & ds] (apply f (if (nil? a) x a) b c ds))))
  ([f x y]
   (fn
     ([a b] (f (if (nil? a) x a) (if (nil? b) y b)))
     ([a b c] (f (if (nil? a) x a) (if (nil? b) y b) c))
     ([a b c & ds] (apply f (if (nil? a) x a) (if (nil? b) y b) c ds))))
  ([f x y z]
   (fn
     ([a b] (f (if (nil? a) x a) (if (nil? b) y b)))
     ([a b c] (f (if (nil? a) x a) (if (nil? b) y b) (if (nil? c) z c)))
     ([a b c & ds] (apply f (if (nil? a) x a) (if (nil? b) y b) (if (nil? c) z c) ds)))))

(defn map-indexed
  "Returns a lazy sequence consisting of the result of applying f to 0
  and the first item of coll, followed by applying f to 1 and the second
  item in coll, etc, until coll is exhausted. Thus function f should
  accept 2 arguments, index and item."
  [f coll]
  (letfn [(mapi [idx coll]
            (lazy-seq
             (when-let [s (seq coll)]
               (if (chunked-seq? s)
                 (let [c (chunk-first s)
                       size (count c)
                       b (chunk-buffer size)]
                   (dotimes [i size]
                     (chunk-append b (f (+ idx i) (-nth c i))))
                   (chunk-cons (chunk b) (mapi (+ idx size) (chunk-rest s))))
                 (cons (f idx (first s)) (mapi (inc idx) (rest s)))))))]
    (mapi 0 coll)))

(defn keep
  "Returns a lazy sequence of the non-nil results of (f item). Note,
  this means false return values will be included.  f must be free of
  side-effects."
  ([f coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (if (chunked-seq? s)
        (let [c (chunk-first s)
              size (count c)
              b (chunk-buffer size)]
          (dotimes [i size]
            (let [x (f (-nth c i))]
              (when-not (nil? x)
                (chunk-append b x))))
          (chunk-cons (chunk b) (keep f (chunk-rest s))))
        (let [x (f (first s))]
          (if (nil? x)
            (keep f (rest s))
            (cons x (keep f (rest s))))))))))

(defn keep-indexed
  "Returns a lazy sequence of the non-nil results of (f index item). Note,
  this means false return values will be included.  f must be free of
  side-effects."
  ([f coll]
     (letfn [(keepi [idx coll]
               (lazy-seq
                (when-let [s (seq coll)]
                  (if (chunked-seq? s)
                    (let [c (chunk-first s)
                          size (count c)
                          b (chunk-buffer size)]
                      (dotimes [i size]
                        (let [x (f (+ idx i) (-nth c i))]
                          (when-not (nil? x)
                            (chunk-append b x))))
                      (chunk-cons (chunk b) (keepi (+ idx size) (chunk-rest s))))
                    (let [x (f idx (first s))]
                      (if (nil? x)
                        (keepi (inc idx) (rest s))
                        (cons x (keepi (inc idx) (rest s)))))))))]
       (keepi 0 coll))))

(defn every-pred
  "Takes a set of predicates and returns a function f that returns true if all of its
  composing predicates return a logical true value against all of its arguments, else it returns
  false. Note that f is short-circuiting in that it will stop execution on the first
  argument that triggers a logical false result against the original predicates."
  ([p]
     (fn ep1
       ([] true)
       ([x] (boolean (p x)))
       ([x y] (boolean (and (p x) (p y))))
       ([x y z] (boolean (and (p x) (p y) (p z))))
       ([x y z & args] (boolean (and (ep1 x y z)
                                     (every? p args))))))
  ([p1 p2]
     (fn ep2
       ([] true)
       ([x] (boolean (and (p1 x) (p2 x))))
       ([x y] (boolean (and (p1 x) (p1 y) (p2 x) (p2 y))))
       ([x y z] (boolean (and (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z))))
       ([x y z & args] (boolean (and (ep2 x y z)
                                     (every? #(and (p1 %) (p2 %)) args))))))
  ([p1 p2 p3]
     (fn ep3
       ([] true)
       ([x] (boolean (and (p1 x) (p2 x) (p3 x))))
       ([x y] (boolean (and (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y))))
       ([x y z] (boolean (and (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y) (p1 z) (p2 z) (p3 z))))
       ([x y z & args] (boolean (and (ep3 x y z)
                                     (every? #(and (p1 %) (p2 %) (p3 %)) args))))))
  ([p1 p2 p3 & ps]
     (let [ps (list* p1 p2 p3 ps)]
       (fn epn
         ([] true)
         ([x] (every? #(% x) ps))
         ([x y] (every? #(and (% x) (% y)) ps))
         ([x y z] (every? #(and (% x) (% y) (% z)) ps))
         ([x y z & args] (boolean (and (epn x y z)
                                       (every? #(every? % args) ps))))))))

(defn some-fn
  "Takes a set of predicates and returns a function f that returns the first logical true value
  returned by one of its composing predicates against any of its arguments, else it returns
  logical false. Note that f is short-circuiting in that it will stop execution on the first
  argument that triggers a logical true result against the original predicates."
  ([p]
     (fn sp1
       ([] nil)
       ([x] (p x))
       ([x y] (or (p x) (p y)))
       ([x y z] (or (p x) (p y) (p z)))
       ([x y z & args] (or (sp1 x y z)
                           (some p args)))))
  ([p1 p2]
     (fn sp2
       ([] nil)
       ([x] (or (p1 x) (p2 x)))
       ([x y] (or (p1 x) (p1 y) (p2 x) (p2 y)))
       ([x y z] (or (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z)))
       ([x y z & args] (or (sp2 x y z)
                           (some #(or (p1 %) (p2 %)) args)))))
  ([p1 p2 p3]
     (fn sp3
       ([] nil)
       ([x] (or (p1 x) (p2 x) (p3 x)))
       ([x y] (or (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y)))
       ([x y z] (or (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y) (p1 z) (p2 z) (p3 z)))
       ([x y z & args] (or (sp3 x y z)
                           (some #(or (p1 %) (p2 %) (p3 %)) args)))))
  ([p1 p2 p3 & ps]
     (let [ps (list* p1 p2 p3 ps)]
       (fn spn
         ([] nil)
         ([x] (some #(% x) ps))
         ([x y] (some #(or (% x) (% y)) ps))
         ([x y z] (some #(or (% x) (% y) (% z)) ps))
         ([x y z & args] (or (spn x y z)
                             (some #(some % args) ps)))))))

(defn map
  "Returns a lazy sequence consisting of the result of applying f to the
  set of first items of each coll, followed by applying f to the set
  of second items in each coll, until any one of the colls is
  exhausted.  Any remaining items in other colls are ignored. Function
  f should accept number-of-colls arguments."
  ([f coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (if (chunked-seq? s)
        (let [c (chunk-first s)
              size (count c)
              b (chunk-buffer size)]
          (dotimes [i size]
              (chunk-append b (f (-nth c i))))
          (chunk-cons (chunk b) (map f (chunk-rest s))))
        (cons (f (first s)) (map f (rest s)))))))
  ([f c1 c2]
   (lazy-seq
    (let [s1 (seq c1) s2 (seq c2)]
      (when (and s1 s2)
        (cons (f (first s1) (first s2))
              (map f (rest s1) (rest s2)))))))
  ([f c1 c2 c3]
   (lazy-seq
    (let [s1 (seq c1) s2 (seq c2) s3 (seq c3)]
      (when (and  s1 s2 s3)
        (cons (f (first s1) (first s2) (first s3))
              (map f (rest s1) (rest s2) (rest s3)))))))
  ([f c1 c2 c3 & colls]
   (let [step (fn step [cs]
                 (lazy-seq
                  (let [ss (map seq cs)]
                    (when (every? identity ss)
                      (cons (map first ss) (step (map rest ss)))))))]
     (map #(apply f %) (step (conj colls c3 c2 c1))))))

(defn take
  "Returns a lazy sequence of the first n items in coll, or all items if
  there are fewer than n."
  [n coll]
  (lazy-seq
   (when (pos? n)
     (when-let [s (seq coll)]
      (cons (first s) (take (dec n) (rest s)))))))

(defn drop
  "Returns a lazy sequence of all but the first n items in coll."
  [n coll]
  (let [step (fn [n coll]
               (let [s (seq coll)]
                 (if (and (pos? n) s)
                   (recur (dec n) (rest s))
                   s)))]
    (lazy-seq (step n coll))))

(defn drop-last
  "Return a lazy sequence of all but the last n (default 1) items in coll"
  ([s] (drop-last 1 s))
  ([n s] (map (fn [x _] x) s (drop n s))))

(defn take-last
  "Returns a seq of the last n items in coll.  Depending on the type
  of coll may be no better than linear time.  For vectors, see also subvec."
  [n coll]
  (loop [s (seq coll), lead (seq (drop n coll))]
    (if lead
      (recur (next s) (next lead))
      s)))

(defn drop-while
  "Returns a lazy sequence of the items in coll starting from the first
  item for which (pred item) returns nil."
  [pred coll]
  (let [step (fn [pred coll]
               (let [s (seq coll)]
                 (if (and s (pred (first s)))
                   (recur pred (rest s))
                   s)))]
    (lazy-seq (step pred coll))))

(defn cycle
  "Returns a lazy (infinite!) sequence of repetitions of the items in coll."
  [coll] (lazy-seq
          (when-let [s (seq coll)]
            (concat s (cycle s)))))

(defn split-at
  "Returns a vector of [(take n coll) (drop n coll)]"
  [n coll]
  [(take n coll) (drop n coll)])

(defn repeat
  "Returns a lazy (infinite!, or length n if supplied) sequence of xs."
  ([x] (lazy-seq (cons x (repeat x))))
  ([n x] (take n (repeat x))))

(defn replicate
  "Returns a lazy seq of n xs."
  [n x] (take n (repeat x)))

(defn repeatedly
  "Takes a function of no args, presumably with side effects, and
  returns an infinite (or length n if supplied) lazy sequence of calls
  to it"
  ([f] (lazy-seq (cons (f) (repeatedly f))))
  ([n f] (take n (repeatedly f))))

(defn iterate
  "Returns a lazy sequence of x, (f x), (f (f x)) etc. f must be free of side-effects"
  {:added "1.0"}
  [f x] (cons x (lazy-seq (iterate f (f x)))))

(defn interleave
  "Returns a lazy seq of the first item in each coll, then the second etc."
  ([c1 c2]
     (lazy-seq
      (let [s1 (seq c1) s2 (seq c2)]
        (when (and s1 s2)
          (cons (first s1) (cons (first s2)
                                 (interleave (rest s1) (rest s2))))))))
  ([c1 c2 & colls]
     (lazy-seq
      (let [ss (map seq (conj colls c2 c1))]
        (when (every? identity ss)
          (concat (map first ss) (apply interleave (map rest ss))))))))

(defn interpose
  "Returns a lazy seq of the elements of coll separated by sep"
  [sep coll] (drop 1 (interleave (repeat sep) coll)))



(defn- flatten1
  "Take a collection of collections, and return a lazy seq
  of items from the inner collection"
  [colls]
  (let [cat (fn cat [coll colls]
              (lazy-seq
                (if-let [coll (seq coll)]
                  (cons (first coll) (cat (rest coll) colls))
                  (when (seq colls)
                    (cat (first colls) (rest colls))))))]
    (cat nil colls)))

(defn mapcat
  "Returns the result of applying concat to the result of applying map
  to f and colls.  Thus function f should return a collection."
  ([f coll]
    (flatten1 (map f coll)))
  ([f coll & colls]
    (flatten1 (apply map f coll colls))))

(defn filter
  "Returns a lazy sequence of the items in coll for which
  (pred item) returns true. pred must be free of side-effects."
  ([pred coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (if (chunked-seq? s)
        (let [c (chunk-first s)
              size (count c)
              b (chunk-buffer size)]
          (dotimes [i size]
              (when (pred (-nth c i))
                (chunk-append b (-nth c i))))
          (chunk-cons (chunk b) (filter pred (chunk-rest s))))
        (let [f (first s) r (rest s)]
          (if (pred f)
            (cons f (filter pred r))
            (filter pred r))))))))

(defn remove
  "Returns a lazy sequence of the items in coll for which
  (pred item) returns false. pred must be free of side-effects."
  [pred coll]
  (filter (complement pred) coll))

(defn tree-seq
  "Returns a lazy sequence of the nodes in a tree, via a depth-first walk.
   branch? must be a fn of one arg that returns true if passed a node
   that can have children (but may not).  children must be a fn of one
   arg that returns a sequence of the children. Will only be called on
   nodes for which branch? returns true. Root is the root node of the
  tree."
   [branch? children root]
   (let [walk (fn walk [node]
                (lazy-seq
                 (cons node
                  (when (branch? node)
                    (mapcat walk (children node))))))]
     (walk root)))

(defn flatten
  "Takes any nested combination of sequential things (lists, vectors,
  etc.) and returns their contents as a single, flat sequence.
  (flatten nil) returns nil."
  [x]
  (filter #(not (sequential? %))
          (rest (tree-seq sequential? seq x))))

(defn into
  "Returns a new coll consisting of to-coll with all of the items of
  from-coll conjoined."
  [to from]
  (if (satisfies? IEditableCollection to)
    (persistent! (reduce -conj! (transient to) from))
    (reduce -conj to from)))

(defn mapv
  "Returns a vector consisting of the result of applying f to the
  set of first items of each coll, followed by applying f to the set
  of second items in each coll, until any one of the colls is
  exhausted.  Any remaining items in other colls are ignored. Function
  f should accept number-of-colls arguments."
  ([f coll]
     (-> (reduce (fn [v o] (conj! v (f o))) (transient []) coll)
         persistent!))
  ([f c1 c2]
     (into [] (map f c1 c2)))
  ([f c1 c2 c3]
     (into [] (map f c1 c2 c3)))
  ([f c1 c2 c3 & colls]
     (into [] (apply map f c1 c2 c3 colls))))

(defn filterv
  "Returns a vector of the items in coll for which
  (pred item) returns true. pred must be free of side-effects."
  [pred coll]
  (-> (reduce (fn [v o] (if (pred o) (conj! v o) v))
              (transient [])
              coll)
      persistent!))

(defn partition
  "Returns a lazy sequence of lists of n items each, at offsets step
  apart. If step is not supplied, defaults to n, i.e. the partitions
  do not overlap. If a pad collection is supplied, use its elements as
  necessary to complete last partition upto n items. In case there are
  not enough padding elements, return a partition with less than n items."
  ([n coll]
     (partition n n coll))
  ([n step coll]
     (lazy-seq
       (when-let [s (seq coll)]
         (let [p (take n s)]
           (when (== n (count p))
             (cons p (partition n step (drop step s))))))))
  ([n step pad coll]
     (lazy-seq
       (when-let [s (seq coll)]
         (let [p (take n s)]
           (if (== n (count p))
             (cons p (partition n step pad (drop step s)))
             (list (take n (concat p pad)))))))))

(defn get-in
  "Returns the value in a nested associative structure,
  where ks is a sequence of keys. Returns nil if the key is not present,
  or the not-found value if supplied."
  {:added "1.2"
   :static true}
  ([m ks]
     (reduce get m ks))
  ([m ks not-found]
     (loop [sentinel lookup-sentinel
            m m
            ks (seq ks)]
       (if ks
         (if (not (satisfies? ILookup m))
           not-found
           (let [m (get m (first ks) sentinel)]
             (if (identical? sentinel m)
               not-found
               (recur sentinel m (next ks)))))
         m))))

(defn assoc-in
  "Associates a value in a nested associative structure, where ks is a
  sequence of keys and v is the new value and returns a new nested structure.
  If any levels do not exist, hash-maps will be created."
  [m [k & ks] v]
  (if ks
    (assoc m k (assoc-in (get m k) ks v))
    (assoc m k v)))

(defn update-in
  "'Updates' a value in a nested associative structure, where ks is a
  sequence of keys and f is a function that will take the old value
  and any supplied args and return the new value, and returns a new
  nested structure.  If any levels do not exist, hash-maps will be
  created."
  ([m [k & ks] f & args]
   (if ks
     (assoc m k (apply update-in (get m k) ks f args))
     (assoc m k (apply f (get m k) args)))))

;;; DEPRECATED
;;; in favor of PersistentVector

(declare empty-vector)
(deftype Vector [meta array ^:mutable __hash]
  IWithMeta
  (-with-meta [coll meta] (Vector. meta array __hash))

  IMeta
  (-meta [coll] meta)

  IStack
  (-peek [coll]
    (let [count (alength array)]
      (when (> count 0)
        (aget array (dec count)))))
  (-pop [coll]
    (if (> (alength array) 0)
      (let [new-array (aclone array)]
        (scm* [new-array] (subvector new-array 0 (vector-length new-array)))
        (Vector. meta new-array nil))
      (throw (Error. "Can't pop empty vector"))))

  ICollection
  (-conj [coll o]
    (let [new-array (scm* [array o] (vector-append array (vector o)))] 
      (Vector. meta new-array nil)))

  IEmptyableCollection
  (-empty [coll] (with-meta empty-vector meta))

  ISequential
  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  IHash
  (-hash [coll] (caching-hash coll hash-coll __hash))

  ISeqable
  (-seq [coll]
    (when (> (alength array) 0)
      (let [vector-seq
            (fn vector-seq [i]
              (lazy-seq
                (when (< i (alength array))
                  (cons (aget array i) (vector-seq (inc i))))))]
        (vector-seq 0))))

  IPairable
  (-pair [this] (scm* [array] (vector->list array)))

  ICounted
  (-count [coll] (alength array))

  IIndexed
  (-nth
    ([coll n] (aget array n))
    ([coll n not-found]
       (if (and (<= 0 n) (< n (alength array)))
         (aget array n)
         not-found)))

  ILookup
  (-lookup
    ([coll k] (-nth coll k nil))
    ([coll k not-found] (-nth coll k not-found)))
  

  IAssociative
  (-assoc [coll k v]
    (let [new-array (aclone array)]
      (aset new-array k v)
      (Vector. meta new-array nil)))

  IVector
  (-assoc-n [coll n val] (-assoc coll n val))

  IReduce
  (-reduce
    ([v f]
       (ci-reduce array f))
    ([v f start]
       (ci-reduce array f start)))

  IFn
  (-invoke [coll [k not-found]]
    (if (nil? not-found)
      (-lookup coll k)
      (-lookup coll k not-found))))

(def empty-vector (Vector. nil (array) 0))

(defn vector-from-array [xs] (Vector. nil xs nil))

(defn vec [coll]
  (reduce conj empty-vector coll)) ; using [] here causes infinite recursion

;;; PersistentVector

(deftype VectorNode [edit arr])

(defn- pv-fresh-node [edit]
  (VectorNode. edit (make-array 32)))

(defn- pv-aget [node idx]
  (aget (.-arr node) idx))

(defn- pv-aset [node idx val]
  (aset (.-arr node) idx val))

(defn- pv-clone-node [node]
  (VectorNode. (.-edit node) (aclone (.-arr node))))

(defn- tail-off [pv]
  (let [cnt (.-cnt pv)]
    (if (< cnt 32)
      0
      (bit-shift-left (bit-shift-right-zero-fill (dec cnt) 5) 5))))

(defn- new-path [edit level node]
  (loop [ll level
         ret node]
    (if (zero? ll)
      ret
      (let [embed ret
            r (pv-fresh-node edit)
            _ (pv-aset r 0 embed)]
        (recur (- ll 5) r)))))

(defn- push-tail [pv level parent tailnode]
  (let [ret (pv-clone-node parent)
        subidx (bit-and (bit-shift-right-zero-fill (dec (.-cnt pv)) level) 0x01f)]
    (if (== 5 level)
      (do
        (pv-aset ret subidx tailnode)
        ret)
      (let [child (pv-aget parent subidx)]
        (if-not (nil? child)
          (let [node-to-insert (push-tail pv (- level 5) child tailnode)]
            (pv-aset ret subidx node-to-insert)
            ret)
          (let [node-to-insert (new-path nil (- level 5) tailnode)]
            (pv-aset ret subidx node-to-insert)
            ret))))))

(defn- array-for [pv i]
  (if (and (<= 0 i) (< i (.-cnt pv)))
    (if (>= i (tail-off pv))
      (.-tail pv)
      (loop [node (.-root pv)
             level (.-shift pv)]
        (if (pos? level)
          (recur (pv-aget node (bit-and (bit-shift-right-zero-fill i level) 0x01f))
                 (- level 5))
          (.-arr node))))
    (throw (js/Error. (str "No item " i " in vector of length " (.-cnt pv))))))

(defn- do-assoc [pv level node i val]
  (let [ret (pv-clone-node node)]
    (if (zero? level)
      (do
        (pv-aset ret (bit-and i 0x01f) val)
        ret)
      (let [subidx (bit-and (bit-shift-right-zero-fill i level) 0x01f)]
        (pv-aset ret subidx (do-assoc pv (- level 5) (pv-aget node subidx) i val))
        ret))))

(defn- pop-tail [pv level node]
  (let [subidx (bit-and (bit-shift-right-zero-fill (- (.-cnt pv) 2) level) 0x01f)]
    (cond
     (> level 5) (let [new-child (pop-tail pv (- level 5) (pv-aget node subidx))]
                   (if (and (nil? new-child) (zero? subidx))
                     nil
                     (let [ret (pv-clone-node node)]
                       (pv-aset ret subidx new-child)
                       ret)))
     (zero? subidx) nil
     :else (let [ret (pv-clone-node node)]
             (pv-aset ret subidx nil)
             ret))))

(declare tv-editable-root tv-editable-tail TransientVector deref
         pr-sequential pr-sequential-writer pr-writer chunked-seq
         PersistentVector-EMPTY_NODE PersistentVector-EMPTY)

(deftype PersistentVector [meta cnt shift root tail ^:mutable __hash]
  IWithMeta
  (-with-meta [coll meta] (PersistentVector. meta cnt shift root tail __hash))

  IMeta
  (-meta [coll] meta)

  IStack
  (-peek [coll]
    (when (> cnt 0)
      (-nth coll (dec cnt))))
  (-pop [coll]
    (cond
     (zero? cnt) (throw (Error. "Can't pop empty vector"))
     (== 1 cnt) (-with-meta PersistentVector-EMPTY meta)
     (< 1 (- cnt (tail-off coll)))
      (PersistentVector. meta (dec cnt) shift root (slice-pop tail) nil)
      :else (let [new-tail (array-for coll (- cnt 2))
                  nr (pop-tail coll shift root)
                  new-root (if (nil? nr) PersistentVector-EMPTY_NODE nr)
                  cnt-1 (dec cnt)]
              (if (and (< 5 shift) (nil? (pv-aget new-root 1)))
                (PersistentVector. meta cnt-1 (- shift 5) (pv-aget new-root 0) new-tail nil)
                (PersistentVector. meta cnt-1 shift new-root new-tail nil)))))

  ICollection
  (-conj [coll o]
    (if (< (- cnt (tail-off coll)) 32)
      (let [new-tail (aclone-push tail o)]
        (PersistentVector. meta (inc cnt) shift root new-tail nil))
      (let [root-overflow? (> (bit-shift-right-zero-fill cnt 5) (bit-shift-left 1 shift))
            new-shift (if root-overflow? (+ shift 5) shift)
            new-root (if root-overflow?
                       (let [n-r (pv-fresh-node nil)]
                           (pv-aset n-r 0 root)
                           (pv-aset n-r 1 (new-path nil shift (VectorNode. nil tail)))
                           n-r)
                       (push-tail coll shift root (VectorNode. nil tail)))]
        (PersistentVector. meta (inc cnt) new-shift new-root (array o) nil))))

  IEmptyableCollection
  (-empty [coll] (with-meta empty-vector meta))

  ISequential
  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  IHash
  (-hash [coll] (caching-hash coll hash-coll __hash))

  ISeqable
  (-seq [coll]
    (if (zero? cnt)
      nil
      (chunked-seq coll 0 0)))

  ICounted
  (-count [coll] cnt)

  IIndexed
  (-nth [coll n]
    (aget (array-for coll n) (bit-and n 0x01f)))
  (-nth [coll n not-found]
    (if (and (<= 0 n) (< n cnt))
      (-nth coll n)
      not-found))

  ILookup
  (-lookup [coll k] (-nth coll k nil))
  (-lookup [coll k not-found] (-nth coll k not-found))

  IMapEntry
  (-key [coll]
    (-nth coll 0))
  (-val [coll]
    (-nth coll 1))

  IAssociative
  (-assoc [coll k v]
    (cond
       (and (<= 0 k) (< k cnt))
       (if (<= (tail-off coll) k)
         (let [new-tail (aclone tail)]
           (aset new-tail (bit-and k 0x01f) v)
           (PersistentVector. meta cnt shift root new-tail nil))
         (PersistentVector. meta cnt shift (do-assoc coll shift root k v) tail nil))
       (== k cnt) (-conj coll v)
       :else (throw (js/Error. (str "Index " k " out of bounds  [0," cnt "]")))))

  IVector
  (-assoc-n [coll n val] (-assoc coll n val))

  IReduce
  (-reduce [v f]
    (ci-reduce v f))
  (-reduce [v f start]
    (ci-reduce v f start))

  IKVReduce
  (-kv-reduce [v f init]
    (let [step-init (array 0 init)] ; [step 0 init init]
      (loop [i 0]
        (if (< i cnt)
          (let [arr (array-for v i)
                len (alength arr)]
            (let [init (loop [j 0 init (aget step-init 1)]
                         (if (< j len)
                           (let [init (f init (+ j i) (aget arr j))]
                             (if (reduced? init)
                               init
                               (recur (inc j) init)))
                           (do (aset step-init 0 len)
                               (aset step-init 1 init)
                               init)))]
              (if (reduced? init)
                @init
                (recur (+ i (aget step-init 0))))))
          (aget step-init 1)))))

  IFn
  (-invoke [coll [k not-found]]
    (-lookup coll k not-found))

  IEditableCollection
  (-as-transient [coll]
    (TransientVector. cnt shift (tv-editable-root root) (tv-editable-tail tail)))

  IReversible
  (-rseq [coll]
    (if (pos? cnt)
      (RSeq. coll (dec cnt) nil)
      ())))

(defn PersistentVector-fromArray
  [xs no-clone]
  (let [l (alength xs)
        xs (if (identical? no-clone true) xs (aclone xs))]
    (if (< l 32)
      (PersistentVector. nil l 5 PersistentVector-EMPTY_NODE xs nil)
      (let [node (slice xs 0 32)
            v (PersistentVector. nil 32 5 PersistentVector-EMPTY_NODE node nil)]
        (loop [i 32 out (-as-transient v)]
          (if (< i l)
            (recur (inc i) (conj! out (aget xs i)))
            (persistent! out)))))))

(defn vec [coll]
  (-persistent!
   (reduce -conj!
           (-as-transient PersistentVector-EMPTY)
           coll)))

(defn vector [& args] (vec args))

(deftype ChunkedSeq [vec node i off meta ^:mutable __hash]
  IWithMeta
  (-with-meta [coll m]
    (chunked-seq vec node i off m))
  (-meta [coll] meta)

  ISeqable
  (-seq [coll] coll)

  ISequential
  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  ASeq
  ISeq
  (-first [coll]
    (aget node off))
  (-rest [coll]
    (if (< (inc off) (alength node))
      (let [s (chunked-seq vec node i (inc off))]
        (if (nil? s)
          ()
          s))
      (-chunked-rest coll)))

  INext
  (-next [coll]
    (if (< (inc off) (alength node))
      (let [s (chunked-seq vec node i (inc off))]
        (if (nil? s)
          nil
          s))
      (-chunked-next coll)))

  ICollection
  (-conj [coll o]
    (cons o coll))

  IEmptyableCollection
  (-empty [coll]
    (with-meta PersistentVector-EMPTY meta))

  IChunkedSeq
  (-chunked-first [coll]
    (array-chunk node off))
  (-chunked-rest [coll]
    (let [l (alength node)
          s (when (< (+ i l) (-count vec))
              (chunked-seq vec (+ i l) 0))]
      (if (nil? s)
        ()
        s)))

  IChunkedNext
  (-chunked-next [coll]
    (let [l (alength node)
          s (when (< (+ i l) (-count vec))
              (chunked-seq vec (+ i l) 0))]
      (if (nil? s)
        nil
        s)))
  IHash
  (-hash [coll] (caching-hash coll hash-coll __hash)))

(defn chunked-seq
  ([vec i off] (chunked-seq vec (array-for vec i) i off nil))
  ([vec node i off] (chunked-seq vec node i off nil))
  ([vec node i off meta]
     (ChunkedSeq. vec node i off meta nil)))

(declare build-subvec)

(deftype Subvec [meta v start end ^:mutable __hash]
  IWithMeta
  (-with-meta [coll meta] (build-subvec meta v start end __hash))

  IMeta
  (-meta [coll] meta)

  IStack
  (-peek [coll]
    (-nth v (dec end)))
  (-pop [coll]
    (if (== start end)
      (throw (js/Error. "Can't pop empty vector"))
      (build-subvec meta v start (dec end) nil)))

  ICollection
  (-conj [coll o]
    (build-subvec meta (-assoc-n v end o) start (inc end) nil))

  IEmptyableCollection
  (-empty [coll] (with-meta cljscm.core.Vector/EMPTY meta))

  ISequential
  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  IHash
  (-hash [coll] (caching-hash coll hash-coll __hash))

  ISeqable
  (-seq [coll]
    (let [subvec-seq (fn subvec-seq [i]
                       (when-not (== i end)
                         (cons (-nth v i)
                               (lazy-seq
                                (subvec-seq (inc i))))))]
      (subvec-seq start)))

  IPairable
  (-pair [this] (scm* [v start end] (vector->list (subvector v start end))))

  ICounted
  (-count [coll] (- end start))

  IIndexed
  (-nth
    ([coll n]
       (nth v (+ start n)))
    ([coll n not-found]
           (nth v (+ start n) not-found)))

  ILookup
  (-lookup
    ([coll k] (-nth coll k nil))
    ([coll k not-found] (-nth coll k not-found)))
  

  IAssociative
  (-assoc [coll key val]
    (let [v-pos (+ start key)]
      (build-subvec meta (-assoc v v-pos val)
               start (max end (inc v-pos))
               nil)))

  IVector
  (-assoc-n [coll n val] (-assoc coll n val))

  IReduce
  (-reduce
    ([coll f]
       (ci-reduce coll f))
    ([coll f start]
       (ci-reduce coll f start)))
  

  IFn
  (-invoke [coll [k not-found]]
    (if (nil? not-found)
      (-lookup coll k)
      (-lookup coll k not-found))))

(defn- build-subvec [meta v start end __hash]
  (let [c (count v)]
       (when (or (neg? start)
                 (neg? end)
                 (> start c)
                 (> end c))
         (throw (js/Error. "Index out of bounds")))
       (Subvec. meta v start end __hash)))

(defn subvec
  "Returns a persistent vector of the items in vector from
  start (inclusive) to end (exclusive).  If end is not supplied,
  defaults to (count vector). This operation is O(1) and very fast, as
  the resulting vector shares structure with the original and no
  trimming is done."
  ([v start]
     (subvec v start (count v)))
  ([v start end]
     (build-subvec nil v start end nil)))

(defn- tv-ensure-editable [edit node]
  (if (identical? edit (.-edit node))
    node
    (VectorNode. edit (aclone (.-arr node)))))

(defn- tv-editable-root [node]
  (VectorNode. (js-obj) (aclone (.-arr node))))

(defn- tv-editable-tail [tl]
  (let [ret (make-array 32)]
    (array-copy tl 0 ret 0 (alength tl))
    ret))

(defn- tv-push-tail [tv level parent tail-node]
  (let [ret    (tv-ensure-editable (.. tv -root -edit) parent)
        subidx (bit-and (bit-shift-right-zero-fill (dec (.-cnt tv)) level) 0x01f)]
    (pv-aset ret subidx
             (if (== level 5)
               tail-node
               (let [child (pv-aget ret subidx)]
                 (if-not (nil? child)
                   (tv-push-tail tv (- level 5) child tail-node)
                   (new-path (.. tv -root -edit) (- level 5) tail-node)))))
    ret))

(defn- tv-pop-tail [tv level node]
  (let [node   (tv-ensure-editable (.. tv -root -edit) node)
        subidx (bit-and (bit-shift-right-zero-fill (- (.-cnt tv) 2) level) 0x01f)]
    (cond
      (> level 5) (let [new-child (tv-pop-tail
                                   tv (- level 5) (pv-aget node subidx))]
                    (if (and (nil? new-child) (zero? subidx))
                      nil
                      (do (pv-aset node subidx new-child)
                          node)))
      (zero? subidx) nil
      :else (do (pv-aset node subidx nil)
                node))))

(defn- editable-array-for [tv i]
  (if (and (<= 0 i) (< i (.-cnt tv)))
    (if (>= i (tail-off tv))
      (.-tail tv)
      (let [root (.-root tv)]
        (loop [node  root
               level (.-shift tv)]
          (if (pos? level)
            (recur (tv-ensure-editable
                    (.-edit root)
                    (pv-aget node
                             (bit-and (bit-shift-right-zero-fill i level)
                                      0x01f)))
                   (- level 5))
            (.-arr node)))))
    (throw (js/Error.
            (str "No item " i " in transient vector of length " (.-cnt tv))))))

(deftype TransientVector [^:mutable cnt
                          ^:mutable shift
                          ^:mutable root
                          ^:mutable tail]
  ITransientCollection
  (-conj! [tcoll o]
    (if ^boolean (.-edit root)
      (if (< (- cnt (tail-off tcoll)) 32)
        (do (aset tail (bit-and cnt 0x01f) o)
            (set! cnt (inc cnt))
            tcoll)
        (let [tail-node (VectorNode. (.-edit root) tail)
              new-tail  (make-array 32)]
          (aset new-tail 0 o)
          (set! tail new-tail)
          (if (> (bit-shift-right-zero-fill cnt 5)
                 (bit-shift-left 1 shift))
            (let [new-root-array (make-array 32)
                  new-shift      (+ shift 5)]
              (aset new-root-array 0 root)
              (aset new-root-array 1 (new-path (.-edit root) shift tail-node))
              (set! root  (VectorNode. (.-edit root) new-root-array))
              (set! shift new-shift)
              (set! cnt   (inc cnt))
              tcoll)
            (let [new-root (tv-push-tail tcoll shift root tail-node)]
              (set! root new-root)
              (set! cnt  (inc cnt))
              tcoll))))
      (throw (js/Error. "conj! after persistent!"))))

  (-persistent! [tcoll]
    (if ^boolean (.-edit root)
      (do (set! (.-edit root) nil)
          (let [len (- cnt (tail-off tcoll))
                trimmed-tail (make-array len)]
            (array-copy tail 0 trimmed-tail 0 len)
            (PersistentVector. nil cnt shift root trimmed-tail nil)))
      (throw (js/Error. "persistent! called twice"))))

  ITransientAssociative
  (-assoc! [tcoll key val] (-assoc-n! tcoll key val))

  ITransientVector
  (-assoc-n! [tcoll n val]
    (if ^boolean (.-edit root)
      (cond
        (and (<= 0 n) (< n cnt))
        (if (<= (tail-off tcoll) n)
          (do (aset tail (bit-and n 0x01f) val)
              tcoll)
          (let [new-root
                ((fn go [level node]
                   (let [node (tv-ensure-editable (.-edit root) node)]
                     (if (zero? level)
                       (do (pv-aset node (bit-and n 0x01f) val)
                           node)
                       (let [subidx (bit-and (bit-shift-right-zero-fill n level)
                                             0x01f)]
                         (pv-aset node subidx
                                  (go (- level 5) (pv-aget node subidx)))
                         node))))
                 shift root)]
            (set! root new-root)
            tcoll))
        (== n cnt) (-conj! tcoll val)
        :else
        (throw
         (js/Error.
          (str "Index " n " out of bounds for TransientVector of length" cnt))))
      (throw (js/Error. "assoc! after persistent!"))))

  (-pop! [tcoll]
    (if ^boolean (.-edit root)
      (cond
        (zero? cnt) (throw (js/Error. "Can't pop empty vector"))
        (== 1 cnt)                       (do (set! cnt 0) tcoll)
        (pos? (bit-and (dec cnt) 0x01f)) (do (set! cnt (dec cnt)) tcoll)
        :else
        (let [new-tail (editable-array-for tcoll (- cnt 2))
              new-root (let [nr (tv-pop-tail tcoll shift root)]
                         (if-not (nil? nr)
                           nr
                           (VectorNode. (.-edit root) (make-array 32))))]
          (if (and (< 5 shift) (nil? (pv-aget new-root 1)))
            (let [new-root (tv-ensure-editable (.-edit root) (pv-aget new-root 0))]
              (set! root  new-root)
              (set! shift (- shift 5))
              (set! cnt   (dec cnt))
              (set! tail  new-tail)
              tcoll)
            (do (set! root new-root)
                (set! cnt  (dec cnt))
                (set! tail new-tail)
                tcoll))))
      (throw (js/Error. "pop! after persistent!"))))

  ICounted
  (-count [coll]
    (if ^boolean (.-edit root)
      cnt
      (throw (js/Error. "count after persistent!"))))

  IIndexed
  (-nth [coll n]
    (if ^boolean (.-edit root)
      (aget (array-for coll n) (bit-and n 0x01f))
      (throw (js/Error. "nth after persistent!"))))

  (-nth [coll n not-found]
    (if (and (<= 0 n) (< n cnt))
      (-nth coll n)
      not-found))

  ILookup
  (-lookup [coll k] (-nth coll k nil))

  (-lookup [coll k not-found] (-nth coll k not-found))

  IFn
  (-invoke [coll [k not-found]]
    (-lookup coll k not-found)))

;;; PersistentQueue ;;;

(deftype PersistentQueueSeq [meta front rear ^:mutable __hash]
  IWithMeta
  (-with-meta [coll meta] (PersistentQueueSeq. meta front rear __hash))

  IMeta
  (-meta [coll] meta)

  ISeq
  (-first [coll] (-first front))
  (-rest  [coll]
    (if-let [f1 (next front)]
      (PersistentQueueSeq. meta f1 rear nil)
      (if (nil? rear)
        (-empty coll)
        (PersistentQueueSeq. meta rear nil nil))))

  ICollection
  (-conj [coll o] (cons o coll))

  IEmptyableCollection
  (-empty [coll] (with-meta empty-list meta))

  ISequential
  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  IHash
  (-hash [coll] (caching-hash coll hash-coll __hash))

  ISeqable
  (-seq [coll] coll))

(declare empty-persistent-queue)
(deftype PersistentQueue [meta count front rear ^:mutable __hash]
  IWithMeta
  (-with-meta [coll meta] (PersistentQueue. meta count front rear __hash))

  IMeta
  (-meta [coll] meta)

  ISeq
  (-first [coll] (first front))
  (-rest [coll] (rest (seq coll)))

  IStack
  (-peek [coll] (-first front))
  (-pop [coll]
    (if front
      (if-let [f1 (next front)]
        (PersistentQueue. meta (dec count) f1 rear nil)
        (PersistentQueue. meta (dec count) (seq rear) [] nil))
      coll))

  ICollection
  (-conj [coll o]
    (if front
      (PersistentQueue. meta (inc count) front (conj (or rear []) o) nil)
      (PersistentQueue. meta (inc count) (conj front o) [] nil)))

  IEmptyableCollection
  (-empty [coll] empty-persistent-queue)

  ISequential
  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  IHash
  (-hash [coll] (caching-hash coll hash-coll __hash))

  ISeqable
  (-seq [coll]
    (let [rear (seq rear)]
      (if (or front rear)
        (PersistentQueueSeq. nil front (seq rear) nil))))

  ICounted
  (-count [coll] count))

(def empty-persistent-queue (PersistentQueue. nil 0 nil [] 0))

(deftype NeverEquiv []
  IEquiv
  (-equiv [o other] false))

(def ^:private never-equiv (NeverEquiv.))

(defn- equiv-map
  "Assumes y is a map. Returns true if x equals y, otherwise returns
  false."
  [x y]
  (boolean
    (when (map? y)
      ; assume all maps are counted
      (when (== (count x) (count y))
        (every? identity
                (map (fn [xkv] (= (get y (first xkv) never-equiv)
                                  (second xkv)))
                     x))))))


(defn- scan-array [incr k array]
  (let [len (alength array)]
    (loop [i 0]
      (when (< i len)
        (if (identical? k (aget array i))
          i
          (recur (+ i incr)))))))

; The keys field is an array of all keys of this map, in no particular
; order. Any string, keyword, or symbol key is used as a property name
; to store the value in strobj.  If a key is assoc'ed when that same
; key already exists in strobj, the old value is overwritten. If a
; non-string key is assoc'ed, return a HashMap object instead.

(declare hash-map)
#_(defn- obj-map-compare-keys [a b]
  (let [a (hash a)
        b (hash b)]
    (cond
     (< a b) -1
     (> a b) 1
     :else 0)))

#_(defn- obj-map->hash-map [m k v]
  (let [ks  (.-keys m)
        len (alength ks)
        so  (.-strobj m)
        mm  (meta m)]
    (loop [i   0
           out (transient PersistentHashMap-EMPTY)]
      (if (< i len)
        (let [k (aget ks i)]
          (recur (inc i) (assoc! out k (aget so k))))
        (with-meta (persistent! (assoc! out k v)) mm)))))

;;; ObjMap

#_(defn- obj-clone [obj ks]
  (let [new-obj (js-obj)
        l (alength ks)]
    (loop [i 0]
      (when (< i l)
        (let [k (aget ks i)]
          (aset new-obj k (aget obj k))
          (recur (inc i)))))
    new-obj))

#_(deftype ObjMap [meta keys strobj update-count ^:mutable __hash]
  IWithMeta
  (-with-meta [coll meta] (ObjMap. meta keys strobj update-count __hash))

  IMeta
  (-meta [coll] meta)

  ICollection
  (-conj [coll entry]
    (if (vector? entry)
      (-assoc coll (-nth entry 0) (-nth entry 1))
      (reduce -conj
              coll
              entry)))

  IEmptyableCollection
  (-empty [coll] (with-meta cljscm.core.ObjMap/EMPTY meta))

  IEquiv
  (-equiv [coll other] (equiv-map coll other))

  IHash
  (-hash [coll] (caching-hash coll hash-imap __hash))

  ISeqable
  (-seq [coll]
    (when (pos? (alength keys))
      (map #(vector % (aget strobj %))
           (.sort keys obj-map-compare-keys))))

  ICounted
  (-count [coll] (alength keys))

  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found]
    (if (and ^boolean (goog/isString k)
             (not (nil? (scan-array 1 k keys))))
      (aget strobj k)
      not-found))

  IAssociative
  (-assoc [coll k v]
    (if ^boolean (goog/isString k)
        (if (or (> update-count cljscm.core.ObjMap/HASHMAP_THRESHOLD)
                (>= (alength keys) cljscm.core.ObjMap/HASHMAP_THRESHOLD))
          (obj-map->hash-map coll k v)
          (if-not (nil? (scan-array 1 k keys))
            (let [new-strobj (obj-clone strobj keys)]
              (aset new-strobj k v)
              (ObjMap. meta keys new-strobj (inc update-count) nil)) ; overwrite
            (let [new-strobj (obj-clone strobj keys) ; append
                  new-keys (aclone-push keys k)]
              (aset new-strobj k v)
              (ObjMap. meta new-keys new-strobj (inc update-count) nil))))
        ;; non-string key. game over.
        (obj-map->hash-map coll k v)))
  (-contains-key? [coll k]
    (if (and ^boolean (goog/isString k)
             (not (nil? (scan-array 1 k keys))))
      true
      false))

  IKVReduce
  (-kv-reduce [coll f init]
    (let [len (alength keys)]
      (loop [keys (.sort keys obj-map-compare-keys)
             init init]
        (if (seq keys)
          (let [k (first keys)
                init (f init k (aget strobj k))]
            (if (reduced? init)
              @init
              (recur (rest keys) init)))
          init))))

  IMap
  (-dissoc [coll k]
    (if (and ^boolean (goog/isString k)
             (not (nil? (scan-array 1 k keys))))
      (let [new-keys (aclone keys)
            new-strobj (obj-clone strobj keys)]
        (.splice new-keys (scan-array 1 k new-keys) 1)
        (js-delete new-strobj k)
        (ObjMap. meta new-keys new-strobj (inc update-count) nil))
      coll)) ; key not found, return coll unchanged

  IFn
  (-invoke [coll [k not-found]]
    (-lookup coll k not-found))

  IEditableCollection
  (-as-transient [coll]
    (transient (into (hash-map) coll))))

#_(set! cljscm.core.ObjMap/EMPTY (ObjMap. nil (array) (js-obj) 0 0))

#_(set! cljscm.core.ObjMap/HASHMAP_THRESHOLD 32)

#_(set! cljscm.core.ObjMap/fromObject (fn [ks obj] (ObjMap. nil ks obj 0 nil)))

;;; HashMap
;;; DEPRECATED
;;; in favor of PersistentHashMap

; The keys field is an array of all keys of this map, in no particular
; order. Each key is hashed and the result used as a property name of
; hashobj. Each values in hashobj is actually a bucket in order to handle hash
; collisions. A bucket is an array of alternating keys (not their hashes) and
; vals.
(declare empty-hash-map)
#_(deftype HashMap [meta count hashobj ^:mutable __hash]
  IWithMeta
  (-with-meta [coll meta] (HashMap. meta count hashobj __hash))

  IMeta
  (-meta [coll] meta)

  ICollection
  (-conj [coll entry]
    (if (vector? entry)
      (-assoc coll (-nth entry 0) (-nth entry 1))
      (reduce -conj
              coll
              entry)))

  IEmptyableCollection
  (-empty [coll] (with-meta cljscm.core.HashMap/EMPTY meta))

  IEquiv
  (-equiv [coll other] (equiv-map coll other))

  IHash
  (-hash [coll] (caching-hash coll hash-imap __hash))

  ISeqable
  (-seq [coll]
    (when (pos? count)
      (let [hashes (.sort (js-keys hashobj))]
        (mapcat #(map vec (partition 2 (aget hashobj %)))
                hashes))))

  ICounted
  (-count [coll] count)

  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found]
    (let [bucket (aget hashobj (hash k))
          i (when bucket (scan-array 2 k bucket))]
      (if i
        (aget bucket (inc i))
        not-found)))

  IAssociative
  (-assoc [coll k v]
    (let [h (hash k)
          bucket (aget hashobj h)]
      (if bucket
        (let [new-bucket (aclone bucket)
              new-hashobj (goog.object/clone hashobj)]
          (aset new-hashobj h new-bucket)
          (if-let [i (scan-array 2 k new-bucket)]
            (do                         ; found key, replace
              (aset new-bucket (inc i) v)
              (HashMap. meta count new-hashobj nil))
            (do                         ; did not find key, append
              (.push new-bucket k v)
              (HashMap. meta (inc count) new-hashobj nil))))
        (let [new-hashobj (goog.object/clone hashobj)] ; did not find bucket
          (aset new-hashobj h (array k v))
          (HashMap. meta (inc count) new-hashobj nil)))))
  (-contains-key? [coll k]
    (let [bucket (aget hashobj (hash k))
          i (when bucket (scan-array 2 k bucket))]
      (if i
        true
        false)))

  IMap
  (-dissoc [coll k]
    (let [h (hash k)
          bucket (aget hashobj h)
          i (when bucket (scan-array 2 k bucket))]
      (if (not i)
        coll ; key not found, return coll unchanged
        (let [new-hashobj (goog.object/clone hashobj)]
          (if (> 3 (alength bucket))
            (js-delete new-hashobj h)
            (let [new-bucket (aclone bucket)]
              (.splice new-bucket i 2)
              (aset new-hashobj h new-bucket)))
          (HashMap. meta (dec count) new-hashobj nil)))))

  IFn
  (-invoke [coll [k not-found]]
    (-lookup coll k not-found)))

#_(set! cljscm.core.HashMap/EMPTY (HashMap. nil 0 (js-obj) 0))

#_(set! cljscm.core.HashMap/fromArrays (fn [ks vs]
  (let [len (alength ks)]
    (loop [i 0, out cljscm.core.HashMap/EMPTY]
      (if (< i len)
        (recur (inc i) (assoc out (aget ks i) (aget vs i)))
        out)))))

#_(def empty-hash-map (HashMap. nil (scm* [] (make-table))))
#_(defn hash-map-from-arrays
  [ks vs]
  (let [len (alength ks)]
    (loop [i 0, out empty-hash-map]
      (if (< i len)
        (recur (inc i) (assoc out (aget ks i) (aget vs i)))
        out))))
#_(Old HashMap scm code ((seq table))
       
  IReduce 
  (-reduce
    ([v f] (seq-reduce f v))
    ([v f start] (seq-reduce f start v)))

  ICounted
  (-count [coll] (scm* [table] (table-length table)))

  ILookup
  (-lookup
    ([coll k] (scm* [table k] (table-ref table k)))
    ([coll k not-found]
       (scm* [table k not-found]
             (lambda () (table-ref table k not-found)))))  

  IAssociative
  (-assoc [coll k v]
    (let [newtable (scm* [table] (table-copy table))]
      (scm* [newtable k v] (table-set! newtable k v))
      (HashMap. meta newtable)))
  (-contains-key? [coll k]
    (let [i (-lookup coll k lookup-sentinel)]
      (if (identical? i lookup-sentinel)
        false
        true)))

  IMap
  (-dissoc [coll k]
    (let [newtable (scm* [table] (table-copy table))]
      (scm* [newtable k] (table-set! newtable k))
      (HashMap. meta newtable)))

  IFn
  (-invoke [coll [k not-found]]
    (if (nil? not-found)
      (-lookup coll k)
      (-lookup coll k not-found))))

(extend-type Table
  IWithMeta
  (-with-meta [coll meta] (HashMap. meta coll))

  ICollection
  (-conj [coll entry]
    (if (vector? entry)
      (-assoc coll (-nth entry 0) (-nth entry 1))
      (reduce -conj
              coll
              entry)))

  IEmptyableCollection
  (-empty [coll] (scm* [] (make-table)))

  IEquiv
  (-equiv [coll other] (equiv-map coll other))

  IHash
  (-hash [coll] (scm-equal?-hash coll))

  ISeqable
  (-seq [table]
    (let [lst (map (fn [pr] [(scm* [pr] (car pr)) (scm* [pr] (cdr pr))])
                   (scm* [table] (table->list table)))]
      (if (empty? lst)
        nil
        lst)))

  IReduce 
  (-reduce
    ([v f] (seq-reduce f v))
    ([v f start] (seq-reduce f start v)))

  ICounted
  (-count [table] (scm* [table] (table-length table)))

  ILookup
  (-lookup
    ([table k] (-lookup table k nil))
    ([table k not-found]
       (scm* [table k not-found]
             (table-ref table k not-found))))  

  IAssociative
  (-assoc [table k v]
    (let [newtable (scm* [table] (table-copy table))]
      (scm* [newtable k v] (table-set! newtable k v))
      newtable))
  (-contains-key? [table k]
    (let [i (-lookup table k lookup-sentinel)]
      (if (identical? i lookup-sentinel)
        false
        true)))

  IMap
  (-dissoc [table k]
    (let [newtable (scm* [table] (table-copy table))]
      (scm* [newtable k] (table-set! newtable k))
      newtable))

  IFn
  (-invoke [coll [k not-found]]
    (if (nil? not-found)
      (-lookup coll k)
      (-lookup coll k not-found))))

;;; PersistentArrayMap

(defn- array-map-index-of [m k]
  (let [arr (.-arr m)
        len (alength arr)]
    (loop [i 0]
      (cond
        (<= len i) -1
        (= (aget arr i) k) i
        :else (recur (+ i 2))))))

(defn- array-map-index-of-len [m k len]
  (let [arr (.-arr m)]
    (loop [i 0]
      (cond
        (<= len i) -1
        (= (aget arr i) k) i
        :else (recur (+ i 2))))))

(declare TransientArrayMap PersistentArrayMap-EMPTY PersistentHashMap-EMPTY)

(deftype PersistentArrayMap [meta cnt arr ^:mutable __hash]
  IWithMeta
  (-with-meta [coll meta] (PersistentArrayMap. meta cnt arr __hash))

  IMeta
  (-meta [coll] meta)

  ICollection
  (-conj [coll entry]
    (if (vector? entry)
      (-assoc coll (-nth entry 0) (-nth entry 1))
      (reduce -conj coll entry)))

  IEmptyableCollection
  (-empty [coll] (-with-meta PersistentArrayMap-EMPTY meta))

  IEquiv
  (-equiv [coll other] (equiv-map coll other))

  IHash
  (-hash [coll] (caching-hash coll hash-imap __hash))

  ISeqable
  (-seq [coll]
    (when (pos? cnt)
      (let [len (* 2 cnt)
            array-map-seq
            (fn array-map-seq [i]
              (lazy-seq
               (when (< i len)
                 (cons [(aget arr i) (aget arr (inc i))]
                       (array-map-seq (+ i 2))))))]
        (array-map-seq 0))))

  ICounted
  (-count [coll] cnt)

  ILookup
  (-lookup [coll k]
    (-lookup coll k nil))

  (-lookup [coll k not-found]
    (let [idx (array-map-index-of coll k)]
      (if (== idx -1)
        not-found
        (aget arr (inc idx)))))

  IAssociative
  (-assoc [coll k v]
    (let [idx (array-map-index-of coll k)]
      (cond
        (== idx -1)
        (if (< cnt PersistentArrayMap-HASHMAP_THRESHOLD)
          (PersistentArrayMap. meta
                               (inc cnt)
                               (aclone-push2 arr (* cnt 2) k v)
                               nil)
          (with-meta
            (assoc (into PersistentHashMap-EMPTY coll) k v)
            meta))

        (identical? v (aget arr (inc idx)))
        coll

        :else
        (PersistentArrayMap. meta
                             cnt
                             (doto (aclone arr)
                               (aset (inc idx) v))
                             nil))))

  (-contains-key? [coll k]
    (not (== (array-map-index-of coll k) -1)))

  IMap
  (-dissoc [coll k]
    (let [idx (array-map-index-of coll k)]
      (if (>= idx 0)
        (let [len     (alength arr)
              new-len (- len 2)]
          (if (zero? new-len)
            (-empty coll)
            (let [new-arr (make-array new-len)]
              (loop [s 0 d 0]
                (cond
                  (>= s len) (PersistentArrayMap. meta (dec cnt) new-arr nil)
                  (= k (aget arr s)) (recur (+ s 2) d)
                  :else (do (aset new-arr d (aget arr s))
                            (aset new-arr (inc d) (aget arr (inc s)))
                            (recur (+ s 2) (+ d 2))))))))
        coll)))

  IKVReduce
  (-kv-reduce [coll f init]
    (let [len (alength arr)]
      (loop [i 0 init init]
        (if (< i len)
          (let [init (f init (aget arr i) (aget arr (inc i)))]
            (if (reduced? init)
              @init
              (recur (+ i 2) init)))
          init))))

  IFn
  (-invoke [coll [k not-found]]
    (-lookup coll k not-found))

  IEditableCollection
  (-as-transient [coll]
    (TransientArrayMap. (js-obj) (* 2 cnt) (aclone-padded arr (* 2 PersistentArrayMap-HASHMAP_THRESHOLD)))))

(def PersistentArrayMap-HASHMAP_THRESHOLD 16)

(defn PersistentArrayMap-fromArrays
  [ks vs]
  (let [len (count ks)]
    (loop [i   0
           out (transient PersistentArrayMap-EMPTY)]
      (if (< i len)
        (recur (inc i) (assoc! out (aget ks i) (aget vs i)))
        (persistent! out)))))

(declare array->transient-hash-map)

(deftype TransientArrayMap [^:mutable editable?
                            ^:mutable len
                            arr]
  ICounted
  (-count [tcoll]
    (if editable?
      (quot len 2)
      (throw (js/Error. "count after persistent!"))))

  ILookup
  (-lookup [tcoll k]
    (-lookup tcoll k nil))

  (-lookup [tcoll k not-found]
    (if editable?
      (let [idx (array-map-index-of-len tcoll k len)]
        (if (== idx -1)
          not-found
          (aget arr (inc idx))))
      (throw (js/Error. "lookup after persistent!"))))

  ITransientCollection
  (-conj! [tcoll o]
    (if editable?
      (if (satisfies? IMapEntry o)
        (-assoc! tcoll (key o) (val o))
        (loop [es (seq o) tcoll tcoll]
          (if-let [e (first es)]
            (recur (next es)
                   (-assoc! tcoll (key e) (val e)))
            tcoll)))
      (throw (js/Error. "conj! after persistent!"))))

  (-persistent! [tcoll]
    (if editable?
      (do (set! editable? false)
          (PersistentArrayMap. nil (quot len 2) arr nil))
      (throw (js/Error. "persistent! called twice"))))

  ITransientAssociative
  (-assoc! [tcoll key val]
    (if editable?
      (let [idx (array-map-index-of-len tcoll key len)]
        (if (== idx -1)
          (if (<= (+ len 2) (* 2 PersistentArrayMap-HASHMAP_THRESHOLD))
            (do (set! len (+ len 2))
                (aset arr (- len 2) key)
                (aset arr (- len 1) val)
                tcoll)
            (assoc! (array->transient-hash-map len arr) key val))
          (if (identical? val (aget arr (inc idx)))
            tcoll
            (do (aset arr (inc idx) val)
                tcoll))))
      (throw (js/Error. "assoc! after persistent!"))))

  ITransientMap
  (-dissoc! [tcoll key]
    (if editable?
      (let [idx (array-map-index-of-len tcoll key len)]
        (when (>= idx 0)
          (aset arr idx (aget arr (- len 2)))
          (aset arr (inc idx) (aget arr (dec len)))
          (set! len (- len 2)))
        tcoll)
      (throw (js/Error. "dissoc! after persistent!")))))

(declare TransientHashMap)

(defn- array->transient-hash-map [len arr]
  (loop [out (transient {})
         i   0]
    (if (< i len)
      (recur (assoc! out (aget arr i) (aget arr (inc i))) (+ i 2))
      out)))

;;; PersistentHashMap

(deftype Box [^:mutable val])

(declare create-inode-seq create-array-node-seq reset! create-node atom deref)

(defn ^boolean key-test [key other]
  (if ^boolean (scm* [key] (keyword? key))
    (cljscm.core/identical? key other)
    (= key other)))

(defn- mask [hash shift]
  (bit-and (bit-shift-right-zero-fill hash shift) 0x01f))

(defn- clone-and-set
  ([arr i a]
     (doto (aclone arr)
       (aset i a)))
  ([arr i a j b]
     (doto (aclone arr)
       (aset i a)
       (aset j b))))

(defn- remove-pair [arr i]
  (let [new-arr (make-array (- (alength arr) 2))]
    (array-copy arr 0 new-arr 0 (* 2 i))
    (array-copy arr (* 2 (inc i)) new-arr (* 2 i) (- (alength new-arr) (* 2 i)))
    new-arr))

(defn- bitmap-indexed-node-index [bitmap bit]
  (bit-count (bit-and bitmap (dec bit))))

(defn- bitpos [hash shift]
  (bit-shift-left 1 (mask hash shift)))

(defn- edit-and-set
  ([inode edit i a]
     (let [editable (-ensure-editable inode edit)]
       (aset (.-arr editable) i a)
       editable))
  ([inode edit i a j b]
     (let [editable (-ensure-editable inode edit)]
       (aset (.-arr editable) i a)
       (aset (.-arr editable) j b)
       editable)))

(defn- inode-kv-reduce [arr f init]
  (let [len (alength arr)]
    (loop [i 0 init init]
      (if (< i len)
        (let [init (let [k (aget arr i)]
                     (if-not (nil? k)
                       (f init k (aget arr (inc i)))
                       (let [node (aget arr (inc i))]
                         (if-not (nil? node)
                           (-kv-reduce node f init)
                           init))))]
          (if (reduced? init)
            @init
            (recur (+ i 2) init)))
        init))))

(declare ArrayNode)

(deftype BitmapIndexedNode [edit ^:mutable bitmap ^:mutable arr]
  INode
  (-inode-assoc [inode shift hash key val added-leaf?]
    (let [bit (bitpos hash shift)
          idx (bitmap-indexed-node-index bitmap bit)]
      (if (zero? (bit-and bitmap bit))
        (let [n (bit-count bitmap)]
          (if (>= n 16)
            (let [nodes (make-array 32)
                  jdx   (mask hash shift)]
              (aset nodes jdx (-inode-assoc BitmapIndexedNode-EMPTY (+ shift 5) hash key val added-leaf?))
              (loop [i 0 j 0]
                (if (< i 32)
                  (if (zero? (bit-and (bit-shift-right-zero-fill bitmap i) 1))
                    (recur (inc i) j)
                    (do (aset nodes i
                              (if-not (nil? (aget arr j))
                                (-inode-assoc BitmapIndexedNode-EMPTY
                                              (+ shift 5) (cljscm.core/hash (aget arr j)) (aget arr j) (aget arr (inc j)) added-leaf?)
                                (aget arr (inc j))))
                        (recur (inc i) (+ j 2))))))
              (ArrayNode. nil (inc n) nodes))
            (let [new-arr (make-array (* 2 (inc n)))]
              (array-copy arr 0 new-arr 0 (* 2 idx))
              (aset new-arr (* 2 idx) key)
              (aset new-arr (inc (* 2 idx)) val)
              (array-copy arr (* 2 idx) new-arr (* 2 (inc idx)) (* 2 (- n idx)))
              (set! (.-val added-leaf?) true)
              (BitmapIndexedNode. nil (bit-or bitmap bit) new-arr))))
        (let [key-or-nil  (aget arr (* 2 idx))
              val-or-node (aget arr (inc (* 2 idx)))]
          (cond (nil? key-or-nil)
                (let [n (-inode-assoc val-or-node (+ shift 5) hash key val added-leaf?)]
                  (if (identical? n val-or-node)
                    inode
                    (BitmapIndexedNode. nil bitmap (clone-and-set arr (inc (* 2 idx)) n))))

                (key-test key key-or-nil)
                (if (identical? val val-or-node)
                  inode
                  (BitmapIndexedNode. nil bitmap (clone-and-set arr (inc (* 2 idx)) val)))

                :else
                (do (set! (.-val added-leaf?) true)
                    (BitmapIndexedNode. nil bitmap
                                        (clone-and-set arr (* 2 idx) nil (inc (* 2 idx))
                                                       (create-node (+ shift 5) key-or-nil val-or-node hash key val)))))))))

  (-inode-without [inode shift hash key]
    (let [bit (bitpos hash shift)]
      (if (zero? (bit-and bitmap bit))
        inode
        (let [idx         (bitmap-indexed-node-index bitmap bit)
              key-or-nil  (aget arr (* 2 idx))
              val-or-node (aget arr (inc (* 2 idx)))]
          (cond (nil? key-or-nil)
                (let [n (-inode-without val-or-node (+ shift 5) hash key)]
                  (cond (identical? n val-or-node) inode
                        (not (nil? n)) (BitmapIndexedNode. nil bitmap (clone-and-set arr (inc (* 2 idx)) n))
                        (== bitmap bit) nil
                        :else (BitmapIndexedNode. nil (bit-xor bitmap bit) (remove-pair arr idx))))
                (key-test key key-or-nil)
                (BitmapIndexedNode. nil (bit-xor bitmap bit) (remove-pair arr idx))
                :else inode)))))

  (-inode-lookup [inode shift hash key not-found]
    (let [bit (bitpos hash shift)]
      (if (zero? (bit-and bitmap bit))
        not-found
        (let [idx         (bitmap-indexed-node-index bitmap bit)
              key-or-nil  (aget arr (* 2 idx))
              val-or-node (aget arr (inc (* 2 idx)))]
          (cond (nil? key-or-nil)  (-inode-lookup val-or-node (+ shift 5) hash key not-found)
                (key-test key key-or-nil) val-or-node
                :else not-found)))))

  (-inode-find [inode shift hash key not-found]
    (let [bit (bitpos hash shift)]
      (if (zero? (bit-and bitmap bit))
        not-found
        (let [idx         (bitmap-indexed-node-index bitmap bit)
              key-or-nil  (aget arr (* 2 idx))
              val-or-node (aget arr (inc (* 2 idx)))]
          (cond (nil? key-or-nil) (-inode-find val-or-node (+ shift 5) hash key not-found)
                (key-test key key-or-nil)          [key-or-nil val-or-node]
                :else not-found)))))

  (-inode-seq [inode]
    (create-inode-seq arr))

  (-ensure-editable [inode e]
    (if (identical? e edit)
      inode
      (let [n       (bit-count bitmap)
            new-arr (make-array (if (neg? n) 4 (* 2 (inc n))))]
        (array-copy arr 0 new-arr 0 (* 2 n))
        (BitmapIndexedNode. e bitmap new-arr))))

  (-inode-assoc! [inode edit shift hash key val added-leaf?]
    (let [bit (bitpos hash shift)
          idx (bitmap-indexed-node-index bitmap bit)]
      (if (zero? (bit-and bitmap bit))
        (let [n (bit-count bitmap)]
          (cond
            (< (* 2 n) (alength arr))
            (let [editable (-ensure-editable inode edit)
                  earr     (.-arr editable)]
              (set! (.-val added-leaf?) true)
              (array-copy-downward earr (* 2 idx)
                                   earr (* 2 (inc idx))
                                   (* 2 (- n idx)))
              (aset earr (* 2 idx) key)
              (aset earr (inc (* 2 idx)) val)
              (set! (.-bitmap editable) (bit-or (.-bitmap editable) bit))
              editable)

            (>= n 16)
            (let [nodes (make-array 32)
                  jdx   (mask hash shift)]
              (aset nodes jdx (-inode-assoc! BitmapIndexedNode-EMPTY edit (+ shift 5) hash key val added-leaf?))
              (loop [i 0 j 0]
                (if (< i 32)
                  (if (zero? (bit-and (bit-shift-right-zero-fill bitmap i) 1))
                    (recur (inc i) j)
                    (do (aset nodes i
                              (if-not (nil? (aget arr j))
                                (-inode-assoc! BitmapIndexedNode-EMPTY
                                               edit (+ shift 5) (cljscm.core/hash (aget arr j)) (aget arr j) (aget arr (inc j)) added-leaf?)
                                (aget arr (inc j))))
                        (recur (inc i) (+ j 2))))))
              (ArrayNode. edit (inc n) nodes))

            :else
            (let [new-arr (make-array (* 2 (+ n 4)))]
              (array-copy arr 0 new-arr 0 (* 2 idx))
              (aset new-arr (* 2 idx) key)
              (aset new-arr (inc (* 2 idx)) val)
              (array-copy arr (* 2 idx) new-arr (* 2 (inc idx)) (* 2 (- n idx)))
              (set! (.-val added-leaf?) true)
              (let [editable (-ensure-editable inode edit)]
                (set! (.-arr editable) new-arr)
                (set! (.-bitmap editable) (bit-or (.-bitmap editable) bit))
                editable))))
        (let [key-or-nil  (aget arr (* 2 idx))
              val-or-node (aget arr (inc (* 2 idx)))]
          (cond (nil? key-or-nil)
                (let [n (-inode-assoc! val-or-node edit (+ shift 5) hash key val added-leaf?)]
                  (if (identical? n val-or-node)
                    inode
                    (edit-and-set inode edit (inc (* 2 idx)) n)))

                (key-test key key-or-nil)
                (if (identical? val val-or-node)
                  inode
                  (edit-and-set inode edit (inc (* 2 idx)) val))

                :else
                (do (set! (.-val added-leaf?) true)
                    (edit-and-set inode edit (* 2 idx) nil (inc (* 2 idx))
                                  (create-node edit (+ shift 5) key-or-nil val-or-node hash key val))))))))

  (-inode-without! [inode edit shift hash key removed-leaf?]
    (let [bit (bitpos hash shift)]
      (if (zero? (bit-and bitmap bit))
        inode
        (let [idx         (bitmap-indexed-node-index bitmap bit)
              key-or-nil  (aget arr (* 2 idx))
              val-or-node (aget arr (inc (* 2 idx)))
              edit-and-remove-pair (fn [inode e bit i]
                                     (if (== bitmap bit)
                                       nil
                                       (let [editable (-ensure-editable inode e)
                                             earr     (.-arr editable)
                                             len      (alength earr)]
                                         (set! (.-bitmap editable) (bit-xor bit (.-bitmap editable)))
                                         (array-copy earr (* 2 (inc i))
                                                     earr (* 2 i)
                                                     (- len (* 2 (inc i))))
                                         (aset earr (- len 2) nil)
                                         (aset earr (dec len) nil)
                                         editable)))]
          (cond (nil? key-or-nil)
                (let [n (-inode-without! val-or-node edit (+ shift 5) hash key removed-leaf?)]
                  (cond (identical? n val-or-node) inode
                        (not (nil? n)) (edit-and-set inode edit (inc (* 2 idx)) n)
                        (== bitmap bit) nil
                        :else (edit-and-remove-pair inode edit bit idx)))
                (key-test key key-or-nil)
                (do (aset removed-leaf? 0 true)
                    (edit-and-remove-pair inode edit bit idx))
                :else inode)))))
  
  IKVReduce
  (-kv-reduce [inode f init]
    (inode-kv-reduce arr f init)))

(def BitmapIndexedNode-EMPTY (BitmapIndexedNode. nil 0 (make-array 0)))

(defn- pack-array-node [array-node edit idx]
  (let [arr     (.-arr array-node)
        len     (* 2 (dec (.-cnt array-node)))
        new-arr (make-array len)]
    (loop [i 0 j 1 bitmap 0]
      (if (< i len)
        (if (and (not (== i idx))
                 (not (nil? (aget arr i))))
          (do (aset new-arr j (aget arr i))
              (recur (inc i) (+ j 2) (bit-or bitmap (bit-shift-left 1 i))))
          (recur (inc i) j bitmap))
        (BitmapIndexedNode. edit bitmap new-arr)))))

(deftype ArrayNode [edit ^:mutable cnt ^:mutable arr]
  INode
  (-inode-assoc [inode shift hash key val added-leaf?]
    (let [idx  (mask hash shift)
          node (aget arr idx)]
      (if (nil? node)
        (ArrayNode. nil (inc cnt) (clone-and-set arr idx (-inode-assoc BitmapIndexedNode-EMPTY (+ shift 5) hash key val added-leaf?)))
        (let [n (-inode-assoc node (+ shift 5) hash key val added-leaf?)]
          (if (identical? n node)
            inode
            (ArrayNode. nil cnt (clone-and-set arr idx n)))))))

  (-inode-without [inode shift hash key]
    (let [idx  (mask hash shift)
          node (aget arr idx)]
      (if-not (nil? node)
        (let [n (-inode-without node (+ shift 5) hash key)]
          (cond
            (identical? n node)
            inode

            (nil? n)
            (if (<= cnt 8)
              (pack-array-node inode nil idx)
              (ArrayNode. nil (dec cnt) (clone-and-set arr idx n)))

            :else
            (ArrayNode. nil cnt (clone-and-set arr idx n))))
        inode)))

  (-inode-lookup [inode shift hash key not-found]
    (let [idx  (mask hash shift)
          node (aget arr idx)]
      (if-not (nil? node)
        (-inode-lookup node (+ shift 5) hash key not-found)
        not-found)))

  (-inode-find [inode shift hash key not-found]
    (let [idx  (mask hash shift)
          node (aget arr idx)]
      (if-not (nil? node)
        (-inode-find node (+ shift 5) hash key not-found)
        not-found)))

  (-inode-seq [inode]
    (create-array-node-seq arr))

  (-ensure-editable [inode e]
    (if (identical? e edit)
      inode
      (ArrayNode. e cnt (aclone arr))))

  (-inode-assoc! [inode edit shift hash key val added-leaf?]
    (let [idx  (mask hash shift)
          node (aget arr idx)]
      (if (nil? node)
        (let [editable (edit-and-set inode edit idx (-inode-assoc! BitmapIndexedNode-EMPTY edit (+ shift 5) hash key val added-leaf?))]
          (set! (.-cnt editable) (inc (.-cnt editable)))
          editable)
        (let [n (-inode-assoc! node edit (+ shift 5) hash key val added-leaf?)]
          (if (identical? n node)
            inode
            (edit-and-set inode edit idx n))))))

  (-inode-without! [inode edit shift hash key removed-leaf?]
    (let [idx  (mask hash shift)
          node (aget arr idx)]
      (if (nil? node)
        inode
        (let [n (-inode-without! node edit (+ shift 5) hash key removed-leaf?)]
          (cond
            (identical? n node)
            inode

            (nil? n)
            (if (<= cnt 8)
              (pack-array-node inode edit idx)
              (let [editable (edit-and-set inode edit idx n)]
                (set! (.-cnt editable) (dec (.-cnt editable)))
                editable))

            :else
            (edit-and-set inode edit idx n))))))

  IKVReduce
  (-kv-reduce [inode f init]
    (let [len (alength arr)]           ; actually 32
      (loop [i 0 init init]
        (if (< i len)
          (let [node (aget arr i)]
            (if-not (nil? node)
              (let [init (-kv-reduce node f init)]
                (if (reduced? init)
                  @init
                  (recur (inc i) init)))))
          init)))))

(defn- hash-collision-node-find-index [arr cnt key]
  (let [lim (* 2 cnt)]
    (loop [i 0]
      (if (< i lim)
        (if (key-test key (aget arr i))
          i
          (recur (+ i 2)))
        -1))))

(deftype HashCollisionNode [edit
                            ^:mutable collision-hash
                            ^:mutable cnt
                            ^:mutable arr]
  INode
  (-inode-assoc [inode shift hash key val added-leaf?]
    (if (== hash collision-hash)
      (let [idx (hash-collision-node-find-index arr cnt key)]
        (if (== idx -1)
          (let [len (alength arr)
                new-arr (make-array (+ len 2))]
            (array-copy arr 0 new-arr 0 len)
            (aset new-arr len key)
            (aset new-arr (inc len) val)
            (set! (.-val added-leaf?) true)
            (HashCollisionNode. nil collision-hash (inc cnt) new-arr))
          (if (= (aget arr idx) val)
            inode
            (HashCollisionNode. nil collision-hash cnt (clone-and-set arr (inc idx) val)))))
      (-inode-assoc (BitmapIndexedNode. nil (bitpos collision-hash shift) (array nil inode))
                    shift hash key val added-leaf?)))

  (-inode-without [inode shift hash key]
    (let [idx (hash-collision-node-find-index arr cnt key)]
      (cond (== idx -1) inode
            (== cnt 1)  nil
            :else (HashCollisionNode. nil collision-hash (dec cnt) (remove-pair arr (quot idx 2))))))

  (-inode-lookup [inode shift hash key not-found]
    (let [idx (hash-collision-node-find-index arr cnt key)]
      (cond (< idx 0)              not-found
            (key-test key (aget arr idx)) (aget arr (inc idx))
            :else                  not-found)))

  (-inode-find [inode shift hash key not-found]
    (let [idx (hash-collision-node-find-index arr cnt key)]
      (cond (< idx 0)              not-found
            (key-test key (aget arr idx)) [(aget arr idx) (aget arr (inc idx))]
            :else                  not-found)))

  (-inode-seq [inode]
    (create-inode-seq arr))

  (-ensure-editable [inode e]
    (if (identical? e edit)
      inode
      (let [new-arr (make-array (* 2 (inc cnt)))]
        (array-copy arr 0 new-arr 0 (* 2 cnt))
        (HashCollisionNode. e collision-hash cnt new-arr))))

  (-inode-assoc! [inode edit shift hash key val added-leaf?]
    (if (== hash collision-hash)
      (let [idx (hash-collision-node-find-index arr cnt key)
            ensure-editable-array (fn [inode e count array]
                                    (if (identical? e edit)
                                      (do (set! arr array)
                                          (set! cnt count)
                                          inode)
                                      (HashCollisionNode. edit collision-hash count array)))]
        (if (== idx -1)
          (if (> (alength arr) (* 2 cnt))
            (let [editable (edit-and-set inode edit (* 2 cnt) key (inc (* 2 cnt)) val)]
              (set! (.-val added-leaf?) true)
              (set! (.-cnt editable) (inc (.-cnt editable)))
              editable)
            (let [len     (alength arr)
                  new-arr (make-array (+ len 2))]
              (array-copy arr 0 new-arr 0 len)
              (aset new-arr len key)
              (aset new-arr (inc len) val)
              (set! (.-val added-leaf?) true)
              (ensure-editable-array inode edit (inc cnt) new-arr)))
          (if (identical? (aget arr (inc idx)) val)
            inode
            (edit-and-set inode edit (inc idx) val))))
      (-inode-assoc! (BitmapIndexedNode. edit (bitpos collision-hash shift) (array nil inode nil nil))
                     edit shift hash key val added-leaf?)))

  (-inode-without! [inode edit shift hash key removed-leaf?]
    (let [idx (hash-collision-node-find-index arr cnt key)]
      (if (== idx -1)
        inode
        (do (aset removed-leaf? 0 true)
            (if (== cnt 1)
              nil
              (let [editable (-ensure-editable inode edit)
                    earr     (.-arr editable)]
                (aset earr idx (aget earr (- (* 2 cnt) 2)))
                (aset earr (inc idx) (aget earr (dec (* 2 cnt))))
                (aset earr (dec (* 2 cnt)) nil)
                (aset earr (- (* 2 cnt) 2) nil)
                (set! (.-cnt editable) (dec (.-cnt editable)))
                editable))))))

  IKVReduce
  (-kv-reduce [inode f init]
    (inode-kv-reduce arr f init)))

(defn- create-node
  ([shift key1 val1 key2hash key2 val2]
     (let [key1hash (hash key1)]
       (if (== key1hash key2hash)
         (HashCollisionNode. nil key1hash 2 (array key1 val1 key2 val2))
         (let [added-leaf? (Box. false)]
           (-> BitmapIndexedNode-EMPTY
               (-inode-assoc shift key1hash key1 val1 added-leaf?)
               (-inode-assoc shift key2hash key2 val2 added-leaf?))))))
  ([edit shift key1 val1 key2hash key2 val2]
     (let [key1hash (hash key1)]
       (if (== key1hash key2hash)
         (HashCollisionNode. nil key1hash 2 (array key1 val1 key2 val2))
         (let [added-leaf? (Box. false)]
           (-> BitmapIndexedNode-EMPTY
               (-inode-assoc! edit shift key1hash key1 val1 added-leaf?)
               (-inode-assoc! edit shift key2hash key2 val2 added-leaf?)))))))

(deftype NodeSeq [meta nodes i s ^:mutable __hash]
  IMeta
  (-meta [coll] meta)

  IWithMeta
  (-with-meta [coll meta] (NodeSeq. meta nodes i s __hash))

  ICollection
  (-conj [coll o] (cons o coll))

  IEmptyableCollection
  (-empty [coll] (with-meta List-EMPTY meta))

  ICollection
  (-conj [coll o] (cons o coll))

  IEmptyableCollection
  (-empty [coll] (with-meta List-EMPTY meta))

  ISequential
  ISeq
  (-first [coll]
    (if (nil? s)
      [(aget nodes i) (aget nodes (inc i))]
      (first s)))

  (-rest [coll]
    (if (nil? s)
      (create-inode-seq nodes (+ i 2) nil)
      (create-inode-seq nodes i (next s))))

  ISeqable
  (-seq [this] this)

  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  IHash
  (-hash [coll] (caching-hash coll hash-coll __hash)))

(defn- create-inode-seq
  ([nodes]
     (create-inode-seq nodes 0 nil))
  ([nodes i s]
     (if (nil? s)
       (let [len (alength nodes)]
         (loop [j i]
           (if (< j len)
             (if-not (nil? (aget nodes j))
               (NodeSeq. nil nodes j nil nil)
               (if-let [node (aget nodes (inc j))]
                 (if-let [node-seq (-inode-seq node)]
                   (NodeSeq. nil nodes (+ j 2) node-seq nil)
                   (recur (+ j 2)))
                 (recur (+ j 2)))))))
       (NodeSeq. nil nodes i s nil))))

(deftype ArrayNodeSeq [meta nodes i s ^:mutable __hash]
  IMeta
  (-meta [coll] meta)

  IWithMeta
  (-with-meta [coll meta] (ArrayNodeSeq. meta nodes i s __hash))

  ICollection
  (-conj [coll o] (cons o coll))

  IEmptyableCollection
  (-empty [coll] (with-meta List-EMPTY meta))

  ICollection
  (-conj [coll o] (cons o coll))

  IEmptyableCollection
  (-empty [coll] (with-meta List-EMPTY meta))

  ISequential
  ISeq
  (-first [coll] (first s))
  (-rest  [coll] (create-array-node-seq nil nodes i (next s)))

  ISeqable
  (-seq [this] this)

  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  IHash
  (-hash [coll] (caching-hash coll hash-coll __hash)))

(defn- create-array-node-seq
  ([nodes] (create-array-node-seq nil nodes 0 nil))
  ([meta nodes i s]
     (if (nil? s)
       (let [len (alength nodes)]
         (loop [j i]
           (if (< j len)
             (if-let [nj (aget nodes j)]
               (if-let [ns (-inode-seq nj)]
                 (ArrayNodeSeq. meta nodes (inc j) ns nil)
                 (recur (inc j)))
               (recur (inc j))))))
       (ArrayNodeSeq. meta nodes i s nil))))

(deftype PersistentHashMap [meta cnt root ^boolean has-nil? nil-val ^:mutable __hash]
  IWithMeta
  (-with-meta [coll meta] (PersistentHashMap. meta cnt root has-nil? nil-val __hash))

  IMeta
  (-meta [coll] meta)

  ICollection
  (-conj [coll entry]
    (if (vector? entry)
      (-assoc coll (-nth entry 0) (-nth entry 1))
      (reduce -conj coll entry)))

  IEmptyableCollection
  (-empty [coll] (-with-meta PersistentHashMap-EMPTY meta))

  IEquiv
  (-equiv [coll other] (equiv-map coll other))

  IHash
  (-hash [coll] (caching-hash coll hash-imap __hash))

  ISeqable
  (-seq [coll]
    (when (pos? cnt)
      (let [s (if-not (nil? root) (-inode-seq root))]
        (if has-nil?
          (cons [nil nil-val] s)
          s))))

  ICounted
  (-count [coll] cnt)

  ILookup
  (-lookup [coll k]
    (-lookup coll k nil))

  (-lookup [coll k not-found]
    (cond (nil? k)    (if has-nil?
                        nil-val
                        not-found)
          (nil? root) not-found
          :else       (-inode-lookup root 0 (hash k) k not-found)))

  IAssociative
  (-assoc [coll k v]
    (if (nil? k)
      (if (and has-nil? (identical? v nil-val))
        coll
        (PersistentHashMap. meta (if has-nil? cnt (inc cnt)) root true v nil))
      (let [added-leaf? (Box. false)
            new-root    (-> (if (nil? root)
                              BitmapIndexedNode-EMPTY
                              root)
                            (-inode-assoc 0 (hash k) k v added-leaf?))]
        (if (identical? new-root root)
          coll
          (PersistentHashMap. meta (if ^boolean (.-val added-leaf?) (inc cnt) cnt) new-root has-nil? nil-val nil)))))

  (-contains-key? [coll k]
    (cond (nil? k)    has-nil?
          (nil? root) false
          :else       (not (identical? (-inode-lookup root 0 (hash k) k lookup-sentinel)
                                       lookup-sentinel))))

  IMap
  (-dissoc [coll k]
    (cond (nil? k)    (if has-nil?
                        (PersistentHashMap. meta (dec cnt) root false nil nil)
                        coll)
          (nil? root) coll
          :else
          (let [new-root (-inode-without root 0 (hash k) k)]
            (if (identical? new-root root)
              coll
              (PersistentHashMap. meta (dec cnt) new-root has-nil? nil-val nil)))))

  IKVReduce
  (-kv-reduce [coll f init]
    (let [init (if has-nil? (f init nil nil-val) init)]
      (cond
        (reduced? init)          @init
        (not (nil? root)) (-kv-reduce root f init)
        :else                    init)))

  IFn
  (-invoke [coll [k not-found]]
    (-lookup coll k not-found))

  IEditableCollection
  (-as-transient [coll]
    (TransientHashMap. (js-obj) root cnt has-nil? nil-val)))

(defn PersistentHashMap-fromArrays
  [ks vs]
  (let [len (alength ks)]
    (loop [i 0 out (transient PersistentHashMap-EMPTY)]
      (if (< i len)
        (recur (inc i) (assoc! out (aget ks i) (aget vs i)))
        (persistent! out)))))

(deftype TransientHashMap [^:mutable ^boolean edit
                           ^:mutable root
                           ^:mutable count
                           ^:mutable ^boolean has-nil?
                           ^:mutable nil-val]
  ICounted
  (-count [coll]
    (if edit
      count
      (throw (js/Error. "count after persistent!"))))

  ILookup
  (-lookup [tcoll k]
    (if (nil? k)
      (if has-nil?
        nil-val)
      (if (nil? root)
        nil
        (-inode-lookup root 0 (hash k) k))))

  (-lookup [tcoll k not-found]
    (if (nil? k)
      (if has-nil?
        nil-val
        not-found)
      (if (nil? root)
        not-found
        (-inode-lookup root 0 (hash k) k not-found))))

  ITransientCollection
  (-conj! [tcoll o]
    (if edit
      (if (satisfies? IMapEntry o)
        (-assoc! tcoll (key o) (val o))
        (loop [es (seq o) tcoll tcoll]
          (if-let [e (first es)]
            (recur (next es)
                   (-assoc! tcoll (key e) (val e)))
            tcoll)))
      (throw (js/Error. "conj! after persistent"))))

  (-persistent! [tcoll]
    (if edit
      (do (set! edit nil)
          (PersistentHashMap. nil count root has-nil? nil-val nil))
      (throw (js/Error. "persistent! called twice"))))

  ITransientAssociative
  (-assoc! [tcoll k v]
    (if edit
      (if (nil? k)
        (do (if (identical? nil-val v)
              nil
              (set! nil-val v))
            (if has-nil?
              nil
              (do (set! count (inc count))
                  (set! has-nil? true)))
            tcoll)
        (let [added-leaf? (Box. false)
              node        (-> (if (nil? root)
                                BitmapIndexedNode-EMPTY
                                root)
                              (-inode-assoc! edit 0 (hash k) k v added-leaf?))]
          (if (identical? node root)
            nil
            (set! root node))
          (if ^boolean (.-val added-leaf?)
            (set! count (inc count)))
          tcoll))
      (throw (js/Error. "assoc! after persistent!"))))

  ITransientMap
  (-dissoc! [tcoll k]
    (if edit
      (if (nil? k)
        (if has-nil?
          (do (set! has-nil? false)
              (set! nil-val nil)
              (set! count (dec count))
              tcoll)
          tcoll)
        (if (nil? root)
          tcoll
          (let [removed-leaf? (Box. false)
                node (-inode-without! root edit 0 (hash k) k removed-leaf?)]
            (if (identical? node root)
              nil
              (set! root node))
            (if (aget removed-leaf? 0)
              (set! count (dec count)))
            tcoll)))
      (throw (js/Error. "dissoc! after persistent!")))))

;;; PersistentTreeMap

(defn- tree-map-seq-push [node stack ^boolean ascending?]
  (loop [t node stack stack]
    (if-not (nil? t)
      (recur (if ascending? (.-left t) (.-right t))
             (conj stack t))
      stack)))

(deftype PersistentTreeMapSeq [meta stack ^boolean ascending? cnt ^:mutable __hash]
  ISeqable
  (-seq [this] this)

  ISequential
  ISeq
  (-first [this] (peek stack))
  (-rest [this]
    (let [t (first stack)
          next-stack (tree-map-seq-push (if ascending? (.-right t) (.-left t))
                                        (next stack)
                                        ascending?)]
      (if-not (nil? next-stack)
        (PersistentTreeMapSeq. nil next-stack ascending? (dec cnt) nil)
        ())))

  ICounted
  (-count [coll]
    (if (neg? cnt)
      (inc (count (next coll)))
      cnt))

  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  ICollection
  (-conj [coll o] (cons o coll))

  IEmptyableCollection
  (-empty [coll] (with-meta List-EMPTY meta))

  IHash
  (-hash [coll] (caching-hash coll hash-coll __hash))

  IMeta
  (-meta [coll] meta)

  IWithMeta
  (-with-meta [coll meta]
    (PersistentTreeMapSeq. meta stack ascending? cnt __hash)))

(defn- create-tree-map-seq [tree ascending? cnt]
  (PersistentTreeMapSeq. nil (tree-map-seq-push tree nil ascending?) ascending? cnt nil))

(declare RedNode BlackNode)

(defn- balance-left [key val ins right]
  (if (instance? RedNode ins)
    (cond
      (instance? RedNode (.-left ins))
      (RedNode. (.-key ins) (.-val ins)
              (-blacken (.-left ins))
              (BlackNode. key val (.-right ins) right nil)
              nil)

      (instance? RedNode (.-right ins))
      (RedNode. (.. ins -right -key) (.. ins -right -val)
                (BlackNode. (.-key ins) (.-val ins)
                            (.-left ins)
                            (.. ins -right -left)
                            nil)
                (BlackNode. key val
                            (.. ins -right -right)
                            right
                            nil)
                nil)

      :else
      (BlackNode. key val ins right nil))
    (BlackNode. key val ins right nil)))

(defn- balance-right [key val left ins]
  (if (instance? RedNode ins)
    (cond
      (instance? RedNode (.-right ins))
      (RedNode. (.-key ins) (.-val ins)
                (BlackNode. key val left (.-left ins) nil)
                (-blacken (.-right ins))
                nil)

      (instance? RedNode (.-left ins))
      (RedNode. (.. ins -left -key) (.. ins -left -val)
                (BlackNode. key val left (.. ins -left -left) nil)
                (BlackNode. (.-key ins) (.-val ins)
                            (.. ins -left -right)
                            (.-right ins)
                            nil)
                nil)

      :else
      (BlackNode. key val left ins nil))
    (BlackNode. key val left ins nil)))

(defn- balance-left-del [key val del right]
  (cond
    (instance? RedNode del)
    (RedNode. key val (-blacken del) right nil)

    (instance? BlackNode right)
    (balance-right key val del (-redden right))

    (and (instance? RedNode right) (instance? BlackNode (.-left right)))
    (RedNode. (.. right -left -key) (.. right -left -val)
              (BlackNode. key val del (.. right -left -left) nil)
              (balance-right (.-key right) (.-val right)
                             (.. right -left -right)
                             (-redden (.-right right)))
              nil)

    :else
    (throw (js/Error. "red-black tree invariant violation"))))

(defn- balance-right-del [key val left del]
  (cond
    (instance? RedNode del)
    (RedNode. key val left (-blacken del) nil)

    (instance? BlackNode left)
    (balance-left key val (-redden left) del)

    (and (instance? RedNode left) (instance? BlackNode (.-right left)))
    (RedNode. (.. left -right -key) (.. left -right -val)
              (balance-left (.-key left) (.-val left)
                            (-redden (.-left left))
                            (.. left -right -left))
              (BlackNode. key val (.. left -right -right) del nil)
              nil)

    :else
    (throw (js/Error. "red-black tree invariant violation"))))

(defn- tree-map-kv-reduce [node f init]
  (let [init (if-not (nil? (.-left node))
               (tree-map-kv-reduce (.-left node) f init)
               init)]
    (if (reduced? init)
      @init
      (let [init (f init (.-key node) (.-val node))]
        (if (reduced? init)
          @init
          (let [init (if-not (nil? (.-right node))
                       (tree-map-kv-reduce (.-right node) f init)
                       init)]
            (if (reduced? init)
              @init
              init)))))))

(deftype BlackNode [key val left right ^:mutable __hash]
  IRedBlackNode
  (-add-left [node ins]
    (-balance-left ins node))

  (-add-right [node ins]
    (-balance-right ins node))

  (-remove-left [node del]
    (balance-left-del key val del right))

  (-remove-right [node del]
    (balance-right-del key val left del))

  (-blacken [node] node)

  (-redden [node] (RedNode. key val left right nil))

  (-balance-left [node parent]
    (BlackNode. (.-key parent) (.-val parent) node (.-right parent) nil))

  (-balance-right [node parent]
    (BlackNode. (.-key parent) (.-val parent) (.-left parent) node nil))

  (-replace [node key val left right]
    (BlackNode. key val left right nil))

  IKVReduce
  (-kv-reduce [node f init]
    (tree-map-kv-reduce node f init))

  IMapEntry
  (-key [node] key)
  (-val [node] val)

  IHash
  (-hash [coll] (caching-hash coll hash-coll __hash))

  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  IMeta
  (-meta [node] nil)

  IWithMeta
  (-with-meta [node meta]
    (with-meta [key val] meta))

  IStack
  (-peek [node] val)

  (-pop [node] [key])

  ICollection
  (-conj [node o] [key val o])

  IEmptyableCollection
  (-empty [node] [])

  ISequential
  ISeqable
  (-seq [node] (list key val))

  ICounted
  (-count [node] 2)

  IIndexed
  (-nth [node n]
    (cond (== n 0) key
          (== n 1) val
          :else    nil))

  (-nth [node n not-found]
    (cond (== n 0) key
          (== n 1) val
          :else    not-found))

  ILookup
  (-lookup [node k] (-nth node k nil))
  (-lookup [node k not-found] (-nth node k not-found))

  IAssociative
  (-assoc [node k v]
    (assoc [key val] k v))

  IVector
  (-assoc-n [node n v]
    (-assoc-n [key val] n v))

  IReduce
  (-reduce [node f]
    (ci-reduce node f))

  (-reduce [node f start]
    (ci-reduce node f start))

  IFn
  (-invoke [node [k not-found]]
    (-lookup node k not-found)))

(deftype RedNode [key val left right ^:mutable __hash]
  IRedBlackNode
  (-add-left [node ins]
    (RedNode. key val ins right nil))

  (-add-right [node ins]
    (RedNode. key val left ins nil))

  (-remove-left [node del]
    (RedNode. key val del right nil))

  (-remove-right [node del]
    (RedNode. key val left del nil))

  (-blacken [node]
    (BlackNode. key val left right nil))

  (-redden [node]
    (throw (js/Error. "red-black tree invariant violation")))

  (-balance-left [node parent]
    (cond
      (instance? RedNode left)
      (RedNode. key val
                (-blacken left)
                (BlackNode. (.-key parent) (.-val parent) right (.-right parent) nil)
                nil)

      (instance? RedNode right)
      (RedNode. (.-key right) (.-val right)
                (BlackNode. key val left (.-left right) nil)
                (BlackNode. (.-key parent) (.-val parent)
                            (.-right right)
                            (.-right parent)
                            nil)
                nil)

      :else
      (BlackNode. (.-key parent) (.-val parent) node (.-right parent) nil)))

  (-balance-right [node parent]
    (cond
      (instance? RedNode right)
      (RedNode. key val
                (BlackNode. (.-key parent) (.-val parent)
                            (.-left parent)
                            left
                            nil)
                (-blacken right)
                nil)

      (instance? RedNode left)
      (RedNode. (.-key left) (.-val left)
                (BlackNode. (.-key parent) (.-val parent)
                            (.-left parent)
                            (.-left left)
                            nil)
                (BlackNode. key val (.-right left) right nil)
                nil)

      :else
      (BlackNode. (.-key parent) (.-val parent) (.-left parent) node nil)))

  (-replace [node key val left right]
    (RedNode. key val left right nil))

  IKVReduce
  (-kv-reduce [node f init]
    (tree-map-kv-reduce node f init))

  IMapEntry
  (-key [node] key)
  (-val [node] val)

  IHash
  (-hash [coll] (caching-hash coll hash-coll __hash))

  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  IMeta
  (-meta [node] nil)

  IWithMeta
  (-with-meta [node meta]
    (with-meta [key val] meta))

  IStack
  (-peek [node] val)

  (-pop [node] [key])

  ICollection
  (-conj [node o] [key val o])

  IEmptyableCollection
  (-empty [node] [])

  ISequential
  ISeqable
  (-seq [node] (list key val))

  ICounted
  (-count [node] 2)

  IIndexed
  (-nth [node n]
    (cond (== n 0) key
          (== n 1) val
          :else    nil))

  (-nth [node n not-found]
    (cond (== n 0) key
          (== n 1) val
          :else    not-found))

  ILookup
  (-lookup [node k] (-nth node k nil))
  (-lookup [node k not-found] (-nth node k not-found))

  IAssociative
  (-assoc [node k v]
    (assoc [key val] k v))

  IVector
  (-assoc-n [node n v]
    (-assoc-n [key val] n v))

  IReduce
  (-reduce [node f]
    (ci-reduce node f))

  (-reduce [node f start]
    (ci-reduce node f start))

  IFn
  (-invoke [node [k not-found]]
    (-lookup node k not-found)))

(defn- tree-map-add [comp tree k v found]
  (if (nil? tree)
    (RedNode. k v nil nil nil)
    (let [c (comp k (.-key tree))]
      (cond
        (zero? c)
        (do (aset found 0 tree)
            nil)

        (neg? c)
        (let [ins (tree-map-add comp (.-left tree) k v found)]
          (if-not (nil? ins)
            (-add-left tree ins)))

        :else
        (let [ins (tree-map-add comp (.-right tree) k v found)]
          (if-not (nil? ins)
            (-add-right tree ins)))))))

(defn- tree-map-append [left right]
  (cond
    (nil? left)
    right

    (nil? right)
    left

    (instance? RedNode left)
    (if (instance? RedNode right)
      (let [app (tree-map-append (.-right left) (.-left right))]
        (if (instance? RedNode app)
          (RedNode. (.-key app) (.-val app)
                    (RedNode. (.-key left) (.-val left)
                              (.-left left)
                              (.-left app)
                              nil)
                    (RedNode. (.-key right) (.-val right)
                              (.-right app)
                              (.-right right)
                              nil)
                    nil)
          (RedNode. (.-key left) (.-val left)
                    (.-left left)
                    (RedNode. (.-key right) (.-val right) app (.-right right) nil)
                    nil)))
      (RedNode. (.-key left) (.-val left)
                (.-left left)
                (tree-map-append (.-right left) right)
                nil))

    (instance? RedNode right)
    (RedNode. (.-key right) (.-val right)
              (tree-map-append left (.-left right))
              (.-right right)
              nil)

    :else
    (let [app (tree-map-append (.-right left) (.-left right))]
      (if (instance? RedNode app)
        (RedNode. (.-key app) (.-val app)
                  (BlackNode. (.-key left) (.-val left)
                              (.-left left)
                              (.-left app)
                              nil)
                  (BlackNode. (.-key right) (.-val right)
                              (.-right app)
                              (.-right right)
                              nil)
                  nil)
        (balance-left-del (.-key left) (.-val left)
                          (.-left left)
                          (BlackNode. (.-key right) (.-val right)
                                      app
                                      (.-right right)
                                      nil))))))

(defn- tree-map-remove [comp tree k found]
  (if-not (nil? tree)
    (let [c (comp k (.-key tree))]
      (cond
        (zero? c)
        (do (aset found 0 tree)
            (tree-map-append (.-left tree) (.-right tree)))

        (neg? c)
        (let [del (tree-map-remove comp (.-left tree) k found)]
          (if (or (not (nil? del)) (not (nil? (aget found 0))))
            (if (instance? BlackNode (.-left tree))
              (balance-left-del (.-key tree) (.-val tree) del (.-right tree))
              (RedNode. (.-key tree) (.-val tree) del (.-right tree) nil))))

        :else
        (let [del (tree-map-remove comp (.-right tree) k found)]
          (if (or (not (nil? del)) (not (nil? (aget found 0))))
            (if (instance? BlackNode (.-right tree))
              (balance-right-del (.-key tree) (.-val tree) (.-left tree) del)
              (RedNode. (.-key tree) (.-val tree) (.-left tree) del nil))))))))

(defn- tree-map-replace [comp tree k v]
  (let [tk (.-key tree)
        c  (comp k tk)]
    (cond (zero? c) (-replace tree tk v (.-left tree) (.-right tree))
          (neg? c)  (-replace tree tk (.-val tree) (tree-map-replace comp (.-left tree) k v) (.-right tree))
          :else     (-replace tree tk (.-val tree) (.-left tree) (tree-map-replace comp (.-right tree) k v)))))

(declare key PersistentTreeMap-EMPTY)

(defn- tree-map-entry-at [coll k]
    (loop [t (.-tree coll)]
      (if-not (nil? t)
        (let [c (comp k (.-key t))]
          (cond (zero? c) t
                (neg? c)  (recur (.-left t))
                :else     (recur (.-right t)))))))

(deftype PersistentTreeMap [comp tree cnt meta ^:mutable __hash]
  IWithMeta
  (-with-meta [coll meta] (PersistentTreeMap. comp tree cnt meta __hash))

  IMeta
  (-meta [coll] meta)

  ICollection
  (-conj [coll entry]
    (if (vector? entry)
      (-assoc coll (-nth entry 0) (-nth entry 1))
      (reduce -conj
              coll
              entry)))

  IEmptyableCollection
  (-empty [coll] (with-meta PersistentTreeMap-EMPTY meta))

  IEquiv
  (-equiv [coll other] (equiv-map coll other))

  IHash
  (-hash [coll] (caching-hash coll hash-imap __hash))

  ICounted
  (-count [coll] cnt)

  IKVReduce
  (-kv-reduce [coll f init]
    (if-not (nil? tree)
      (tree-map-kv-reduce tree f init)
      init))

  IFn
  (-invoke [coll [k not-found]]
    (-lookup coll k not-found))

  ISeqable
  (-seq [coll]
    (if (pos? cnt)
      (create-tree-map-seq tree true cnt)))

  IReversible
  (-rseq [coll]
    (if (pos? cnt)
      (create-tree-map-seq tree false cnt)))

  ILookup
  (-lookup [coll k]
    (-lookup coll k nil))

  (-lookup [coll k not-found]
    (let [n (tree-map-entry-at coll k)]
      (if-not (nil? n)
        (.-val n)
        not-found)))

  IAssociative
  (-assoc [coll k v]
    (let [found (array nil)
          t     (tree-map-add comp tree k v found)]
      (if (nil? t)
        (let [found-node (nth found 0)]
          (if (= v (.-val found-node))
            coll
            (PersistentTreeMap. comp (tree-map-replace comp tree k v) cnt meta nil)))
        (PersistentTreeMap. comp (-blacken t) (inc cnt) meta nil))))

  (-contains-key? [coll k]
    (not (nil? (tree-map-entry-at coll k))))

  IMap
  (-dissoc [coll k]
    (let [found (array nil)
          t     (tree-map-remove comp tree k found)]
      (if (nil? t)
        (if (nil? (nth found 0))
          coll
          (PersistentTreeMap. comp nil 0 meta nil))
        (PersistentTreeMap. comp (-blacken t) (dec cnt) meta nil))))

  ISorted
  (-sorted-seq [coll ascending?]
    (if (pos? cnt)
      (create-tree-map-seq tree ascending? cnt)))

  (-sorted-seq-from [coll k ascending?]
    (if (pos? cnt)
      (loop [stack nil t tree]
        (if-not (nil? t)
          (let [c (comp k (.-key t))]
            (cond
              (zero? c)  (PersistentTreeMapSeq. nil (conj stack t) ascending? -1 nil)
              ascending? (if (neg? c)
                           (recur (conj stack t) (.-left t))
                           (recur stack          (.-right t)))
              :else      (if (pos? c)
                           (recur (conj stack t) (.-right t))
                           (recur stack          (.-left t)))))
          (when-not (nil? stack)
            (PersistentTreeMapSeq. nil stack ascending? -1 nil))))))

  (-entry-key [coll entry] (key entry))

  (-comparator [coll] comp))

(defn hash-map
  "keyval => key val
  Returns a new hash map with supplied mappings."
  [& keyvals]
  (loop [in (seq keyvals), out (transient PersistentHashMap-EMPTY)]
    (if in
      (recur (nnext in) (assoc! out (first in) (second in)))
      (persistent! out))))

(defn array-map
  "keyval => key val
  Returns a new array map with supplied mappings."
  [& keyvals]
  (PersistentArrayMap. nil (quot (count keyvals) 2) (apply array keyvals) nil))

#_(defn obj-map
  "keyval => key val
  Returns a new object map with supplied mappings."
  [& keyvals]
  (let [ks  (array)
        obj (js-obj)]
    (loop [kvs (seq keyvals)]
      (if kvs
        (do (.push ks (first kvs))
            (aset obj (first kvs) (second kvs))
            (recur (nnext kvs)))
        (cljscm.core.ObjMap/fromObject ks obj)))))

(defn sorted-map
  "keyval => key val
  Returns a new sorted map with supplied mappings."
  ([& keyvals]
     (loop [in (seq keyvals) out PersistentTreeMap-EMPTY]
       (if in
         (recur (nnext in) (assoc out (first in) (second in)))
         out))))

(defn sorted-map-by
  "keyval => key val
  Returns a new sorted map with supplied mappings, using the supplied comparator."
  ([comparator & keyvals]
     (loop [in (seq keyvals)
            out (cljscm.core.PersistentTreeMap. (fn->comparator comparator) nil 0 nil 0)]
       (if in
         (recur (nnext in) (assoc out (first in) (second in)))
         out))))

(defn keys
  "Returns a sequence of the map's keys."
  [hash-map]
  (seq (map first hash-map)))

(defn key
  "Returns the key of the map entry."
  [map-entry]
  (-key map-entry))

(defn vals
  "Returns a sequence of the map's values."
  [hash-map]
  (seq (map second hash-map)))

(defn val
  "Returns the value in the map entry."
  [map-entry]
  (-val map-entry))

(defn merge
  "Returns a map that consists of the rest of the maps conj-ed onto
  the first.  If a key occurs in more than one map, the mapping from
  the latter (left-to-right) will be the mapping in the result."
  [& maps]
  (when (some identity maps)
    (reduce #(conj (or %1 {}) %2) maps)))

(defn merge-with
  "Returns a map that consists of the rest of the maps conj-ed onto
  the first.  If a key occurs in more than one map, the mapping(s)
  from the latter (left-to-right) will be combined with the mapping in
  the result by calling (f val-in-result val-in-latter)."
  [f & maps]
  (when (some identity maps)
    (let [merge-entry (fn [m e]
                        (let [k (first e) v (second e)]
                          (if (contains? m k)
                            (assoc m k (f (get m k) v))
                            (assoc m k v))))
          merge2 (fn [m1 m2]
                   (reduce merge-entry (or m1 {}) (seq m2)))]
      (reduce merge2 maps))))

(defn select-keys
  "Returns a map containing only those entries in map whose key is in keys"
  [map keyseq]
    (loop [ret {} keys (seq keyseq)]
      (if keys
        (let [key   (first keys)
              entry (get map key ::not-found)]
          (recur
           (if (not= entry ::not-found)
             (assoc ret key entry)
             ret)
           (next keys)))
        ret)))

;;; PersistentHashSet
(declare TransientHashSet PersistentHashSet-EMPTY)

(deftype PersistentHashSet [meta hash-map ^:mutable __hash]
  IWithMeta
  (-with-meta [coll meta] (PersistentHashSet. meta hash-map __hash))

  IMeta
  (-meta [coll] meta)

  ICollection
  (-conj [coll o]
    (PersistentHashSet. meta (assoc hash-map o nil) nil))

  IEmptyableCollection
  (-empty [coll] (with-meta PersistentHashSet-EMPTY meta))

  IEquiv
  (-equiv [coll other]
    (and
     (set? other)
     (== (count coll) (count other))
     (every? #(contains? coll %)
             other)))

  IHash
  (-hash [coll] (caching-hash coll hash-iset __hash))

  ISeqable
  (-seq [coll] (keys hash-map))

  ICounted
  (-count [coll] (count (seq coll)))

  ILookup
  (-lookup
    ([coll v]
       (-lookup coll v nil))
    ([coll v not-found]
       (if (-contains-key? hash-map v)
         v
         not-found)))

  IReduce 
  (-reduce
    ([v f] (seq-reduce f v))
    ([v f start] (seq-reduce f start v)))

  ISet
  (-disjoin [coll v]
    (PersistentHashSet. meta (dissoc hash-map v) nil))

  IFn
  (-invoke [coll [k not-found]]
    (-lookup coll k not-found))

  IEditableCollection
  (-as-transient [coll] (TransientHashSet. (transient hash-map))))

(defn PersistentHashSet-fromArray
  [items]
  (let [len (count items)]
    (loop [i   0
           out (transient PersistentHashSet-EMPTY)]
      (if (< i len)
        (recur (inc i) (conj! out (aget items i)))
        (persistent! out)))))

(deftype TransientHashSet [^:mutable transient-map]
  ITransientCollection
  (-conj! [tcoll o]
    (set! transient-map (assoc! transient-map o nil))
    tcoll)

  (-persistent! [tcoll]
    (PersistentHashSet. nil (persistent! transient-map) nil))

  ITransientSet
  (-disjoin! [tcoll v]
    (set! transient-map (dissoc! transient-map v))
    tcoll)

  ICounted
  (-count [tcoll] (count transient-map))

  ILookup
  (-lookup [tcoll v]
    (-lookup tcoll v nil))

  (-lookup [tcoll v not-found]
    (if (identical? (-lookup transient-map v lookup-sentinel) lookup-sentinel)
      not-found
      v))

  IFn
  (-invoke [tcoll [k not-found]]
    (if (identical? (-lookup transient-map k lookup-sentinel) lookup-sentinel)
      not-found
      k)))

(declare PersistentTreeSet-EMPTY)

(deftype PersistentTreeSet [meta tree-map ^:mutable __hash]
  IWithMeta
  (-with-meta [coll meta] (PersistentTreeSet. meta tree-map __hash))

  IMeta
  (-meta [coll] meta)

  ICollection
  (-conj [coll o]
    (PersistentTreeSet. meta (assoc tree-map o nil) nil))

  IEmptyableCollection
  (-empty [coll] (with-meta PersistentTreeSet-EMPTY meta))

  IEquiv
  (-equiv [coll other]
    (and
     (set? other)
     (== (count coll) (count other))
     (every? #(contains? coll %)
             other)))

  IHash
  (-hash [coll] (caching-hash coll hash-iset __hash))

  ISeqable
  (-seq [coll] (keys tree-map))

  ISorted
  (-sorted-seq [coll ascending?]
    (map key (-sorted-seq tree-map ascending?)))

  (-sorted-seq-from [coll k ascending?]
    (map key (-sorted-seq-from tree-map k ascending?)))

  (-entry-key [coll entry] entry)

  (-comparator [coll] (-comparator tree-map))

  IReversible
  (-rseq [coll]
    (map key (rseq tree-map)))

  ICounted
  (-count [coll] (count tree-map))

  ILookup
  (-lookup [coll v]
    (-lookup coll v nil))
  (-lookup [coll v not-found]
    (let [n (tree-map-entry-at tree-map v)]
      (if-not (nil? n)
        (.-key n)
        not-found)))

  ISet
  (-disjoin [coll v]
    (PersistentTreeSet. meta (dissoc tree-map v) nil))

  IFn
  (-invoke [coll [k not-found]]
    (if (nil? not-found)
      (-lookup coll k)
      (-lookup coll k not-found))))

(defn hash-set
  ([] PersistentHashSet-EMPTY)
  ([& keys]
     (loop [in (seq keys)
            out (transient PersistentHashSet-EMPTY)]
       (if (seq in)
         (recur (next in) (conj! out (first in)))
         (persistent! out)))))

(defn set
  "Returns a set of the distinct elements of coll."
  [coll]
    (apply hash-set coll))

(defn sorted-set
  "Returns a new sorted set with supplied keys."
  ([& keys]
   (reduce -conj PersistentTreeSet-EMPTY keys)))

(defn sorted-set-by
  "Returns a new sorted set with supplied keys, using the supplied comparator."
  ([comparator & keys]
   (reduce -conj
           (cljscm.core.PersistentTreeSet. nil (sorted-map-by comparator) 0)
           keys)))

(defn replace
  "Given a map of replacement pairs and a vector/collection, returns a
  vector/seq with any elements = a key in smap replaced with the
  corresponding val in smap"
  [smap coll]
  (if (vector? coll)
    (let [n (count coll)]
      (reduce (fn [v i]
                (if-let [e (find smap (nth v i))]
                  (assoc v i (second e))
                  v))
              coll (take n (iterate inc 0))))
    (map #(if-let [e (find smap %)] (second e) %) coll)))

(defn distinct
  "Returns a lazy sequence of the elements of coll with duplicates removed"
  [coll]
  (let [step (fn step [xs seen]
               (lazy-seq
                ((fn [[f :as xs] seen]
                   (when-let [s (seq xs)]
                     (if (contains? seen f)
                       (recur (rest s) seen)
                       (cons f (step (rest s) (conj seen f))))))
                 xs seen)))]
    (step coll #{})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn butlast [s]
  (loop [ret [] s s]
    (if (next s)
      (recur (conj ret (first s)) (next s))
      (seq ret))))

(defn name
  "Returns the namespace String of a symbol or keyword, or nil if not present."
  [s]
  (let [st (if (keyword? s) (drop 1 (str s)) (str s))
        [ns _ nm] (partition-by #{\/} st)]
    (if nm (apply str nm) (apply str ns))))

(defn namespace
  "Returns the namespace String of a symbol or keyword, or nil if not present."
  [s]
  (let [[ns _ nm] (partition-by #{\/} (str s))]
              (when nm (apply str ns))))

(defn zipmap
  "Returns a map with the keys mapped to the corresponding vals."
  [keys vals]
    (loop [map {}
           ks (seq keys)
           vs (seq vals)]
      (if (and ks vs)
        (recur (assoc map (first ks) (first vs))
               (next ks)
               (next vs))
        map)))

(defn max-key
  "Returns the x for which (k x), a number, is greatest."
  ([k x] x)
  ([k x y] (if (> (k x) (k y)) x y))
  ([k x y & more]
   (reduce #(max-key k %1 %2) (max-key k x y) more)))

(defn min-key
  "Returns the x for which (k x), a number, is least."
  ([k x] x)
  ([k x y] (if (< (k x) (k y)) x y))
  ([k x y & more]
     (reduce #(min-key k %1 %2) (min-key k x y) more)))

(defn partition-all
  "Returns a lazy sequence of lists like partition, but may include
  partitions with fewer than n items at the end."
  ([n coll]
     (partition-all n n coll))
  ([n step coll]
     (lazy-seq
      (when-let [s (seq coll)]
        (cons (take n s) (partition-all n step (drop step s)))))))

(defn take-while
  "Returns a lazy sequence of successive items from coll while
  (pred item) returns true. pred must be free of side-effects."
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (when (pred (first s))
       (cons (first s) (take-while pred (rest s)))))))

(defn mk-bound-fn
  [sc test key]
  (fn [e]
    (let [comp (-comparator sc)]
      (test (comp (-entry-key sc e) key) 0))))

(defn subseq
  "sc must be a sorted collection, test(s) one of <, <=, > or
  >=. Returns a seq of those entries with keys ek for
  which (test (.. sc comparator (compare ek key)) 0) is true"
  ([sc test key]
     (let [include (mk-bound-fn sc test key)]
       (if (#{> >=} test)
         (when-let [[e :as s] (-sorted-seq-from sc key true)]
           (if (include e) s (next s)))
         (take-while include (-sorted-seq sc true)))))
  ([sc start-test start-key end-test end-key]
     (when-let [[e :as s] (-sorted-seq-from sc start-key true)]
       (take-while (mk-bound-fn sc end-test end-key)
                   (if ((mk-bound-fn sc start-test start-key) e) s (next s))))))

(defn rsubseq
  "sc must be a sorted collection, test(s) one of <, <=, > or
  >=. Returns a reverse seq of those entries with keys ek for
  which (test (.. sc comparator (compare ek key)) 0) is true"
  ([sc test key]
     (let [include (mk-bound-fn sc test key)]
       (if (#{< <=} test)
         (when-let [[e :as s] (-sorted-seq-from sc key false)]
           (if (include e) s (next s)))
         (take-while include (-sorted-seq sc false)))))
  ([sc start-test start-key end-test end-key]
     (when-let [[e :as s] (-sorted-seq-from sc end-key false)]
       (take-while (mk-bound-fn sc start-test start-key)
                   (if ((mk-bound-fn sc end-test end-key) e) s (next s))))))

(deftype Range [meta start end step ^:mutable __hash]
  IWithMeta
  (-with-meta [rng meta] (Range. meta start end step __hash))

  IMeta
  (-meta [rng] meta)

  ISeqable
  (-seq [rng]
    (if (pos? step)
      (when (< start end)
        rng)
      (when (> start end)
        rng)))

  ISeq
  (-first [rng] start)
  (-rest [rng]
    (if-not (nil? (-seq rng))
      (Range. meta (+ start step) end step nil)
      ()))

  INext
  (-next [rng]
    (if (pos? step)
      (when (< (+ start step) end)
        (Range. meta (+ start step) end step nil))
      (when (> (+ start step) end)
        (Range. meta (+ start step) end step nil))))

  ICollection
  (-conj [rng o] (cons o rng))

  IEmptyableCollection
  (-empty [rng] (with-meta empty-list meta))

  ISequential
  IEquiv
  (-equiv [rng other] (equiv-sequential rng other))

  IHash
  (-hash [rng] (caching-hash rng hash-coll __hash))

  ICounted
  (-count [rng]
    (if-not (-seq rng)
      0
      (scm* [end start step] (ceiling (/ (- end start) step)))))
  
  IIndexed
  (-nth [rng n]
    (if (< n (-count rng))
      (+ start (* n step))
      (if (and (> start end) (zero? step))
        start
        (throw (js/Error. "Index out of bounds")))))
  (-nth [rng n not-found]
    (if (< n (-count rng))
      (+ start (* n step))
      (if (and (> start end) (zero? step))
        start
        not-found)))

  IReduce
  (-reduce
    ([rng f] (ci-reduce rng f))
    ([rng f s] (ci-reduce rng f s))))

(defn range
  "Returns a lazy seq of nums from start (inclusive) to end
   (exclusive), by step, where start defaults to 0, step to 1,
   and end to infinity."
  ([] (range 0 '+inf.0 1)) ;js/Number.MAX_VALUE 1
  ([end] (range 0 end 1))
  ([start end] (range start end 1))
  ([start end step] (Range. nil start end step nil)))

(defn take-nth
  "Returns a lazy seq of every nth item in coll."
  [n coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (cons (first s) (take-nth n (drop n s))))))

(defn split-with
  "Returns a vector of [(take-while pred coll) (drop-while pred coll)]"
  [pred coll]
  [(take-while pred coll) (drop-while pred coll)])

(defn partition-by
  "Applies f to each value in coll, splitting it each time f returns
   a new value.  Returns a lazy seq of partitions."
  [f coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [fst (first s)
           fv (f fst)
           run (cons fst (take-while #(= fv (f %)) (next s)))]
       (cons run (partition-by f (seq (drop (count run) s))))))))

(defn frequencies
  "Returns a map from distinct items in coll to the number of times
  they appear."
  [coll]
  (persistent!
   (reduce (fn [counts x]
             (assoc! counts x (inc (get counts x 0))))
           (transient {}) coll)))

(defn reductions
  "Returns a lazy seq of the intermediate values of the reduction (as
  per reduce) of coll by f, starting with init."
  ([f coll]
     (lazy-seq
      (if-let [s (seq coll)]
        (reductions f (first s) (rest s))
        (list (f)))))
  ([f init coll]
     (cons init
           (lazy-seq
            (when-let [s (seq coll)]
              (reductions f (f init (first s)) (rest s)))))))

(defn juxt
  "Takes a set of functions and returns a fn that is the juxtaposition
  of those fns.  The returned fn takes a variable number of args, and
  returns a vector containing the result of applying each fn to the
  args (left-to-right).
  ((juxt a b c) x) => [(a x) (b x) (c x)]"
  ([f]
     (fn
       ([] (vector (f)))
       ([x] (vector (f x)))
       ([x y] (vector (f x y)))
       ([x y z] (vector (f x y z)))
       ([x y z & args] (vector (apply f x y z args)))))
  ([f g]
     (fn
       ([] (vector (f) (g)))
       ([x] (vector (f x) (g x)))
       ([x y] (vector (f x y) (g x y)))
       ([x y z] (vector (f x y z) (g x y z)))
       ([x y z & args] (vector (apply f x y z args) (apply g x y z args)))))
  ([f g h]
     (fn
       ([] (vector (f) (g) (h)))
       ([x] (vector (f x) (g x) (h x)))
       ([x y] (vector (f x y) (g x y) (h x y)))
       ([x y z] (vector (f x y z) (g x y z) (h x y z)))
       ([x y z & args] (vector (apply f x y z args) (apply g x y z args) (apply h x y z args)))))
  ([f g h & fs]
     (let [fs (list* f g h fs)]
       (fn
         ([] (reduce #(conj %1 (%2)) [] fs))
         ([x] (reduce #(conj %1 (%2 x)) [] fs))
         ([x y] (reduce #(conj %1 (%2 x y)) [] fs))
         ([x y z] (reduce #(conj %1 (%2 x y z)) [] fs))
         ([x y z & args] (reduce #(conj %1 (apply %2 x y z args)) [] fs))))))

(defn dorun
  "When lazy sequences are produced via functions that have side
  effects, any effects other than those needed to produce the first
  element in the seq do not occur until the seq is consumed. dorun can
  be used to force any effects. Walks through the successive nexts of
  the seq, does not retain the head and returns nil."
  ([coll]
   (when (seq coll)
     (recur (next coll))))
  ([n coll]
   (when (and (seq coll) (pos? n))
     (recur (dec n) (next coll)))))

(defn doall
  "When lazy sequences are produced via functions that have side
  effects, any effects other than those needed to produce the first
  element in the seq do not occur until the seq is consumed. doall can
  be used to force any effects. Walks through the successive nexts of
  the seq, retains the head and returns it, thus causing the entire
  seq to reside in memory at one time."
  ([coll]
   (dorun coll)
   coll)
  ([n coll]
   (dorun n coll)
   coll))

;;;;;;;;;;;;;;;;;;;;;;;;; Regular Expressions ;;;;;;;;;;

(comment TODO
(defn regexp? [o]
  (js* "~{o} instanceof RegExp"))

(defn re-matches
  "Returns the result of (re-find re s) if re fully matches s."
  [re s]
  (let [matches (.exec re s)]
    (when (= (first matches) s)
      (if (== (count matches) 1)
        (first matches)
        (vec matches)))))

(defn re-find
  "Returns the first regex match, if any, of s to re, using
  re.exec(s). Returns a vector, containing first the matching
  substring, then any capturing groups if the regular expression contains
  capturing groups."
  [re s]
  (let [matches (.exec re s)]
    (when-not (nil? matches)
      (if (== (count matches) 1)
        (first matches)
        (vec matches)))))

(defn re-seq
  "Returns a lazy sequence of successive matches of re in s."
  [re s]
  (let [match-data (re-find re s)
        match-idx (.search s re)
        match-str (if (coll? match-data) (first match-data) match-data)
        post-match (subs s (+ match-idx (count match-str)))]
    (when match-data (lazy-seq (cons match-data (re-seq re post-match))))))

(defn re-pattern
  "Returns an instance of RegExp which has compiled the provided string."
  [s]
  (let [[_ flags pattern] (re-find #"^(?:\(\?([idmsux]*)\))?(.*)" s)]
    (js/RegExp. pattern flags)))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Printing ;;;;;;;;;;;;;;;;

(defn ^:deprecated pr-sequential
  "Do not use this.  It is kept for backwards compatibility with the
   old IPrintable protocol."
  [print-one begn sep end opts coll]
  (concat [begn]
          (flatten1
            (interpose [sep] (map #(print-one % opts) coll)))
          [end]))

(defn pr-sequential-writer [writer print-one begn sep end opts coll]
  (-write writer begn)
  (when (seq coll)
    (print-one (first coll) writer opts))
  (doseq [o (next coll)]
    (-write writer sep)
    (print-one o writer opts))
  (-write writer end))

(defn write-all [writer & ss]
  (doseq [s ss]
    (-write writer s)))

(declare string-print)

(defn flush [] ;stub
  nil)

; sb is a raw scheme list of strings. Flush condenses this list into a single-element list of all strings appended in one op with no intermediate strings.
(deftype StringBufferWriter [^:mutable sb]
  IWriter
  (-write [_ s] (set! sb (scm* {:sb sb :s s} (cons :s :sb))))
  (-flush [_] (set! sb (scm* {:sb sb} (list (append-strings (reverse :sb))))))

  IStringable
  (-toString [o] (-flush o) (scm* {:sb sb} (car :sb)))

  ICounted
  (-count [_] (reduce + (map count sb))))

(defn string-buffer-writer
  [] (StringBufferWriter. (scm* {} (list))))

#_(defn- ^:deprecated pr-seq
  "Do not use this.  It is kept for backwards compatibility with the
   old IPrintable protocol."
  [obj opts]
  (cond
    (nil? obj) (list "nil")
    :else (concat
            (when (and (get opts :meta)
                       (satisfies? IMeta obj)
                       (meta obj))
              (concat ["^"] (pr-seq (meta obj) opts) [" "]))
            (cond
             ;; handle CLJS ctors
             (and (not (nil? obj))
                  ^boolean (.-cljs$lang$type obj))
             (.cljs$lang$ctorPrSeq obj obj)

             (satisfies? IPrintable obj) (-pr-seq obj opts)

             (regexp? obj) (list "#\"" (.-source obj) "\"")

             :else (list "#<" (str obj) ">")))))

(defn- pr-writer
  "Prefer this to pr-seq, because it makes the printing function
   configurable, allowing efficient implementations such as appending
   to a StringBuffer."
  [obj writer opts]
  (cond
    (nil? obj) (-write writer "nil")
    (undefined? obj) (-write writer "#<undefined>")
    :else (do
            (when (and (get opts :meta)
                       (satisfies? IMeta obj)
                       (meta obj))
              (-write writer "^")
              (pr-writer (meta obj) writer opts)
              (-write writer " "))
            (cond
              ;; handle CLJS ctors
              ;; (and (not (nil? obj))
              ;;      ^boolean (.-cljs$lang$type obj))
              ;;   (.cljs$lang$ctorPrWriter obj obj writer opts)

              ; Use the new, more efficient, IPrintWithWriter interface when possible.
              (satisfies? IPrintWithWriter obj) (-pr-writer obj writer opts)

              ; Fall back on the deprecated IPrintable if necessary.  Note that this
              ; will only happen when ClojureScript users have implemented -pr-seq
              ; for their custom types.
              (satisfies? IPrintable obj) (apply write-all writer (-pr-seq obj opts))

;              (regexp? obj) (write-all writer "#\"" (.-source obj) "\"")

              :else (write-all writer "#<" (str obj) ">")))))

(defn pr-seq-writer [objs writer opts]
  (pr-writer (first objs) writer opts)
  (doseq [obj (next objs)]
    (-write writer " ")
    (pr-writer obj writer opts)))

(defn- pr-sb-with-opts [objs opts]
  (let [writer (string-buffer-writer)]
    (pr-seq-writer objs writer opts)
    (-flush writer)
    (-toString writer)))

(defn pr-str-with-opts
  "Prints a sequence of objects to a string, observing all the
  options given in opts"
  [objs opts]
  (if (empty? objs)
    ""
    (str (pr-sb-with-opts objs opts))))

(defn prn-str-with-opts
  "Same as pr-str-with-opts followed by (newline)"
  [objs opts]
  (if (empty? objs)
    "\n"
    (let [sb (pr-sb-with-opts objs opts)]
      (str sb \newline))))

(defn- pr-with-opts
  "Prints a sequence of objects using string-print, observing all
  the options given in opts"
  [objs opts]
  (string-print (pr-str-with-opts objs opts)))

(defn newline [opts]
  (string-print "\n")
  (when (get opts :flush-on-newline)
    (flush)))

(def *flush-on-newline* true)
(def *print-readably* true)
(def *print-meta* false)
(def *print-dup* false)

(defn- pr-opts []
  {:flush-on-newline *flush-on-newline*
   :readably *print-readably*
   :meta *print-meta*
   :dup *print-dup*})

(defn pr-str
  "pr to a string, returning it. Fundamental entrypoint to IPrintable."
  [& objs]
  (pr-str-with-opts objs (pr-opts)))

(defn prn-str
  "Same as pr-str followed by (newline)"
  [& objs]
  (prn-str-with-opts objs (pr-opts)))

(defn pr
  "Prints the object(s) using string-print.  Prints the
  object(s), separated by spaces if there is more than one.
  By default, pr and prn print in a way that objects can be
  read by the reader"
  [& objs]
  (pr-with-opts objs (pr-opts)))

(def ^{:doc
  "Prints the object(s) using string-print.
  print and println produce output for human consumption."}
  print
  (fn cljs-core-print [& objs]
    (pr-with-opts objs (assoc (pr-opts) :readably false))))

(defn print-str
  "print to a string, returning it"
  [& objs]
  (pr-str-with-opts objs (assoc (pr-opts) :readably false)))

(defn println
  "Same as print followed by (newline)"
  [& objs]
  (pr-with-opts objs (assoc (pr-opts) :readably false))
  (newline (pr-opts)))

(defn println-str
  "println to a string, returning it"
  [& objs]
  (prn-str-with-opts objs (assoc (pr-opts) :readably false)))

(defn prn
  "Same as pr followed by (newline)."
  [& objs]
  (pr-with-opts objs (pr-opts))
  (newline (pr-opts)))

(defn printf
  "Prints formatted output, as per format"
  [fmt & args]
  (print (apply format fmt args)))

#_(def ^:private char-escapes {"\"" "\\\""
                             "\\" "\\\\"
                             "\b" "\\b"
                             "\f" "\\f"
                             "\n" "\\n"
                             "\r" "\\r"
                             "\t" "\\t"})

#_(defn ^:private quote-string
  [s]
  ;;TODO
  s
  #_(str \"
       (.replace s (js/RegExp "[\\\\\"\b\f\n\r\t]" "g")
         (fn [match] (get char-escapes match)))
       \"))

(extend-protocol IPrintWithWriter
  Class
  (-pr-writer [o wr opts] (-write wr (str "#<Class " (scm-unsafe-vector-ref o 2) ">")))
  
  Number
  (-pr-writer [n wr opts] (-write wr (scm* [n] (number->string n))))

  Pair
  (-pr-writer [c wr opts] (if ((scm* {} list?) c)
                            (pr-sequential-writer wr pr-writer  "(" " " ")" opts c)
                            (-write wr ((scm* {} object->string) c)))) ;improper list.

  Boolean
  (-pr-writer [bool wr opts] (-write wr (if bool "true" "false")))

  Nil
  (-pr-writer [o wr _] (-write wr "nil"))

  Null
  (-pr-writer [o wr _] (-write wr "()"))

  Char
  (-pr-writer [s wr opts] (if (:readably opts)
                            (-write wr (str "\\" (scm* [s] (string s))))
                            (-write wr (scm* [s] (string s)))))
  
  Array
  (-pr-writer [a wr opts]
    ^:deprecation-nowarn (pr-sequential-writer wr pr-writer "#<Array [" ", " "]>" opts a))

  Table
  (-pr-writer [coll wr opts]
    (let [pr-pair (fn [keyval wr ops] (pr-sequential-writer wr pr-writer "" " " "" opts keyval))]
      (pr-sequential-writer wr pr-pair "{" ", " "}" opts coll)))

  String
  (-pr-writer [o wr opts] (if (:readably opts)
                            (-write wr (str "\"" o "\""))
                            (-write wr o)))

  ;; string
  ;; (-pr-writer [obj writer opts]
  ;;   (cond
  ;;    (keyword? obj)
  ;;      (do
  ;;        (-write writer ":")
  ;;        (when-let [nspc (namespace obj)]
  ;;          (write-all writer (str nspc) "/"))
  ;;        (-write writer (name obj)))
  ;;    (symbol? obj)
  ;;      (do
  ;;        (when-let [nspc (namespace obj)]
  ;;          (write-all writer (str nspc) "/"))
  ;;        (-write writer (name obj)))
  ;;    :else (if (:readably opts)
  ;;            (-write writer (quote-string obj))
  ;;            (-write writer obj))))

  Symbol
  (-pr-writer [o wr opts] (-write wr (scm* [o] (symbol->string o))))

  Symbol+
  (-pr-writer [o wr opts] (-write wr (scm* {::s (.-s ^Symbol+ o)} (symbol->string ::s))))

  Keyword
  (-pr-writer [k wr opts] (-write wr (str ":" (scm* [k] (keyword->string k)))))

  Procedure
  (-pr-writer [this wr _] (-write wr (scm* [this] (object->string this))))

  Error
  (-pr-writer [this wr _] (-write wr (str "#<Error: "(.-msg this)">")))

  ;; js/Date
  ;; (-pr-writer [d writer _]
  ;;   (let [normalize (fn [n len]
  ;;                     (loop [ns (str n)]
  ;;                       (if (< (count ns) len)
  ;;                         (recur (str "0" ns))
  ;;                         ns)))]
  ;;     (write-all writer
  ;;       "#inst \""
  ;;       (str (.getUTCFullYear d))             "-"
  ;;       (normalize (inc (.getUTCMonth d)) 2)  "-"
  ;;       (normalize (.getUTCDate d) 2)         "T"
  ;;       (normalize (.getUTCHours d) 2)        ":"
  ;;       (normalize (.getUTCMinutes d) 2)      ":"
  ;;       (normalize (.getUTCSeconds d) 2)      "."
  ;;       (normalize (.getUTCMilliseconds d) 3) "-"
  ;;       "00:00\"")))

  LazySeq
  (-pr-writer [coll writer opts] ^:deprecation-nowarn (pr-sequential-writer writer pr-writer "(" " " ")" opts coll))

  IndexedSeq
  (-pr-writer [coll writer opts] ^:deprecation-nowarn (pr-sequential-writer writer pr-writer "(" " " ")" opts coll))

  RSeq
  (-pr-writer [coll writer opts] ^:deprecation-nowarn (pr-sequential-writer writer pr-writer "(" " " ")" opts coll))

  PersistentQueue
  (-pr-writer [coll writer opts] ^:deprecation-nowarn (pr-sequential-writer writer pr-writer "#queue [" " " "]" opts (seq coll)))

  PersistentTreeMapSeq
  (-pr-writer [coll writer opts] ^:deprecation-nowarn (pr-sequential-writer writer pr-writer "(" " " ")" opts coll))

  NodeSeq
  (-pr-writer [coll writer opts] ^:deprecation-nowarn (pr-sequential-writer writer pr-writer "(" " " ")" opts coll))

  ArrayNodeSeq
  (-pr-writer [coll writer opts] ^:deprecation-nowarn (pr-sequential-writer writer pr-writer "(" " " ")" opts coll))

  Cons
  (-pr-writer [coll writer opts] ^:deprecation-nowarn (pr-sequential-writer writer pr-writer "(" " " ")" opts coll))

  EmptyList
  (-pr-writer [coll writer opts] (-write writer "()"))

  Vector
  (-pr-writer [coll writer opts] ^:deprecation-nowarn (pr-sequential-writer writer pr-writer "[" " " "]" opts coll))

  PersistentVector
  (-pr-writer [coll writer opts] ^:deprecation-nowarn (pr-sequential-writer writer pr-writer "[" " " "]" opts coll))

  ChunkedCons
  (-pr-writer [coll writer opts] ^:deprecation-nowarn (pr-sequential-writer writer pr-writer "(" " " ")" opts coll))

  ChunkedSeq
  (-pr-writer [coll writer opts] ^:deprecation-nowarn (pr-sequential-writer writer pr-writer "(" " " ")" opts coll))

  Subvec
  (-pr-writer [coll writer opts] ^:deprecation-nowarn (pr-sequential-writer writer pr-writer "[" " " "]" opts coll))

  BlackNode
  (-pr-writer [coll writer opts] ^:deprecation-nowarn (pr-sequential-writer writer pr-writer "[" " " "]" opts coll))

  RedNode
  (-pr-writer [coll writer opts] ^:deprecation-nowarn (pr-sequential-writer writer pr-writer "[" " " "]" opts coll))

  ;; ObjMap
  ;; (-pr-writer [coll writer opts]
  ;;   (let [pr-pair (fn [keyval] ^:deprecation-nowarn (pr-sequential-writer writer pr-writer "" " " "" opts keyval))]
  ;;    ^:deprecation-nowarn (pr-sequential-writer writer pr-pair "{" ", " "}" opts coll)))

  ;; HashMap
  ;; (-pr-writer [coll writer opts]
  ;;   (let [pr-pair (fn [keyval] ^:deprecation-nowarn (pr-sequential-writer writer pr-writer "" " " "" opts keyval))]
  ;;    ^:deprecation-nowarn (pr-sequential-writer writer pr-pair "{" ", " "}" opts coll)))

  PersistentArrayMap
  (-pr-writer [coll writer opts]
    (let [pr-pair (fn [keyval writer opts] ^:deprecation-nowarn (pr-sequential-writer writer pr-writer "" " " "" opts keyval))]
      ^:deprecation-nowarn (pr-sequential-writer writer pr-pair "{" ", " "}" opts coll)))

  PersistentHashMap
  (-pr-writer [coll writer opts]
    (let [pr-pair (fn [keyval writer opts] ^:deprecation-nowarn (pr-sequential-writer writer pr-writer "" " " "" opts keyval))]
      ^:deprecation-nowarn (pr-sequential-writer writer pr-pair "{" ", " "}" opts coll)))

  PersistentTreeMap
  (-pr-writer [coll writer opts]
    (let [pr-pair (fn [keyval writer opts] ^:deprecation-nowarn (pr-sequential-writer writer pr-writer "" " " "" opts keyval))]
      ^:deprecation-nowarn (pr-sequential-writer writer pr-pair "{" ", " "}" opts coll)))

  PersistentHashSet
  (-pr-writer [coll writer opts] ^:deprecation-nowarn (pr-sequential-writer writer pr-writer "#{" " " "}" opts coll))

  PersistentTreeSet
  (-pr-writer [coll writer opts] ^:deprecation-nowarn (pr-sequential-writer writer pr-writer "#{" " " "}" opts coll))

  Range
  (-pr-writer [coll writer opts] ^:deprecation-nowarn (pr-sequential-writer writer pr-writer "(" " " ")" opts coll)))


;; IComparable
(extend-protocol IComparable
  PersistentVector
  (-compare [x y] (compare-indexed x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Reference Types ;;;;;;;;;;;;;;;;

(deftype Atom [state meta validator watches]
  IEquiv
  (-equiv [o other] (identical? o other))

  IDeref
  (-deref [_] state)

  IMeta
  (-meta [_] meta)

  ;; ^:deprecation-nowarn IPrintable
  ;; (-pr-seq [a opts]
  ;;  (concat  ["#<Atom: "] (pr-seq state opts) [">"]))

  IPrintWithWriter
  (-pr-writer [a writer opts]
    (-write writer "#<Atom: ")
    (-pr-writer state writer opts)
    (-write writer ">"))

  IWatchable
  (-notify-watches [this oldval newval]
    (doseq [[key f] watches]
      (f key this oldval newval)))
  (-add-watch [this key f]
    (let [nw (assoc watches key f)]
      (scm* [this nw] (cljscm.core/Atom-watches-set! this nw))))
  (-remove-watch [this key]
    (let [nw (dissoc watches key)]
      (scm* [this nw] (cljscm.core/Atom-watches-set! this nw))))

  IHash
  (-hash [this] (goog.getUid this)))

(defn atom
  "Creates and returns an Atom with an initial value of x and zero or
  more options (in any order):

  :meta metadata-map

  :validator validate-fn

  If metadata-map is supplied, it will be come the metadata on the
  atom. validate-fn must be nil or a side-effect-free fn of one
  argument, which will be passed the intended new state on any state
  change. If the new state is unacceptable, the validate-fn should
  return false or throw an Error.  If either of these error conditions
  occur, then the value of the atom will not change."
  ([x] (Atom. x nil nil nil))
  ([x & {:keys [meta validator]}] (Atom. x meta validator nil)))

(defn reset!
  "Sets the value of atom to newval without regard for the
  current value. Returns newval."
  [a new-value]
  (when-let [validate (.-validator a)]
    (assert (validate new-value) "Validator rejected reference state"))
  (let [old-value (.-state a)]
    (set! (.-state a) new-value)
    (-notify-watches a old-value new-value))
  new-value)

(defn swap!
  "Atomically swaps the value of atom to be:
  (apply f current-value-of-atom args). Note that f may be called
  multiple times, and thus should be free of side effects.  Returns
  the value that was swapped in."
  ([a f]
     (reset! a (f (.-state a))))
  ([a f x]
     (reset! a (f (.-state a) x)))
  ([a f x y]
     (reset! a (f (.-state a) x y)))
  ([a f x y z]
     (reset! a (f (.-state a) x y z)))
  ([a f x y z & more]
     (reset! a (apply f (.-state a) x y z more))))

(defn compare-and-set!
  "Atomically sets the value of atom to newval if and only if the
  current value of the atom is identical to oldval. Returns true if
  set happened, else false."
  [a oldval newval]
  (if (= (.-state a) oldval)
    (do (reset! a newval) true)
    false))

;; generic to all refs
;; (but currently hard-coded to atom!)

(defn deref
  [o]
  (-deref o))

(defn set-validator!
  "Sets the validator-fn for an atom. validator-fn must be nil or a
  side-effect-free fn of one argument, which will be passed the intended
  new state on any state change. If the new state is unacceptable, the
  validator-fn should return false or throw an Error. If the current state
  is not acceptable to the new validator, an Error will be thrown and the
  validator will not be changed."
  [iref val]
  #_(set! (.-validator iref) val) #_"FIXME: setting field dynamically")

(defn get-validator
  "Gets the validator-fn for a var/ref/agent/atom."
  [iref]
  (.-validator iref))

(defn alter-meta!
  "Atomically sets the metadata for a namespace/var/ref/agent/atom to be:

  (apply f its-current-meta args)

  f must be free of side-effects"
  [iref f & args]
  (set! (.-meta iref) (apply f (.-meta iref) args)) #_"FIXME: setting fields dynamically")

(defn reset-meta!
  "Atomically resets the metadata for an atom"
  [iref m]
  (set! (.-meta iref) m) #_"Setting fields dynamically")

(defn add-watch
  "Alpha - subject to change.

  Adds a watch function to an atom reference. The watch fn must be a
  fn of 4 args: a key, the reference, its old-state, its
  new-state. Whenever the reference's state might have been changed,
  any registered watches will have their functions called. The watch
  fn will be called synchronously. Note that an atom's state
  may have changed again prior to the fn call, so use old/new-state
  rather than derefing the reference. Keys must be unique per
  reference, and can be used to remove the watch with remove-watch,
  but are otherwise considered opaque by the watch mechanism.  Bear in
  mind that regardless of the result or action of the watch fns the
  atom's value will change.  Example:

      (def a (atom 0))
      (add-watch a :inc (fn [k r o n] (assert (== 0 n))))
      (swap! a inc)
      ;; Assertion Error
      (deref a)
      ;=> 1"
  [iref key f]
  (-add-watch iref key f))

(defn remove-watch
  "Alpha - subject to change.

  Removes a watch (set by add-watch) from a reference"
  [iref key]
  (-remove-watch iref key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; gensym ;;;;;;;;;;;;;;;;
;; Internal - do not use!
(def gensym_counter nil)

(defn gensym
  "Returns a new symbol with a unique name. If a prefix string is
  supplied, the name is prefix# where # is some unique number. If
  prefix is not supplied, the prefix is 'G__'."
  ([] (gensym "G__"))
  ([prefix-string]
     (when (nil? gensym_counter)
       (set! gensym_counter (atom 0)))
     (symbol (str prefix-string (swap! gensym_counter inc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Fixtures ;;;;;;;;;;;;;;;;

(def fixture1 1)
(def fixture2 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Delay ;;;;;;;;;;;;;;;;;;;;

(deftype Delay [state f]
  IDeref
  (-deref [_]
    (:value (swap! state (fn [{:keys [done] :as curr-state}]
                           (if done
                             curr-state,
                             {:done true :value (f)})))))

  IPending
  (-realized? [d]
    (:done @state)))

(defn ^boolean delay?
  "returns true if x is a Delay created with delay"
  [x] (instance? cljscm.core.Delay x))

(defn force
  "If x is a Delay, returns the (possibly cached) value of its expression, else returns x"
  [x]
  (if (delay? x)
    (deref x)
    x))

(defn ^boolean realized?
  "Returns true if a value has been produced for a promise, delay, future or lazy sequence."
  [d]
  (-realized? d))

#_(defprotocol IEncodeJS
  (-clj->js [x] "Recursively transforms clj values to JavaScript")
  (-key->js [x] "Transforms map keys to valid JavaScript keys. Arbitrary keys are
  encoded to their string representation via (pr-str x)"))

#_(extend-protocol IEncodeJS
  default
  (-key->js [k]
    (if (or (string? k)
            (number? k)
            (keyword? k)
            (symbol? k))
      (-clj->js k)
      (pr-str k)))

  (-clj->js [x]
    (cond
      (keyword? x) (name x)
      (symbol? x) (str x)
      (map? x) (let [m (js-obj)]
                 (doseq [[k v] x]
                   (aset m (-key->js k) (-clj->js v)))
                 m)
      (coll? x) (apply array (map -clj->js x))
      :else x))

  nil
  (-clj->js [x] nil))

#_(defn clj->js
   "Recursively transforms ClojureScript values to JavaScript.
sets/vectors/lists become Arrays, Keywords and Symbol become Strings,
Maps become Objects. Arbitrary keys are encoded to by key->js."
   [x]
   (-clj->js x))

#_(defprotocol IEncodeClojure
  (-js->clj [x] [x options] "Transforms JavaScript values to Clojure"))

#_(extend-protocol IEncodeClojure
  default
  (-js->clj
    ([x options]
       (let [{:keys [keywordize-keys]} options
             keyfn (if keywordize-keys keyword str)
             f (fn thisfn [x]
                 (cond
                   (seq? x) (doall (map thisfn x))
                   (coll? x) (into (empty x) (map thisfn x))
                   (goog.isArray x) (vec (map thisfn x))
                   (identical? (type x) js/Object) (into {} (for [k (js-keys x)]
                                                              [(keyfn k)
                                                               (thisfn (aget x k))]))
                   :else x))]
         (f x)))
    ([x] (-js->clj x {:keywordize-keys false}))))

#_(defn js->clj
  "Recursively transforms JavaScript arrays into ClojureScript
  vectors, and JavaScript objects into ClojureScript maps.  With
  option ':keywordize-keys true' will convert object fields from
  strings to keywords."
  [x & opts]
  (-js->clj x (apply array-map opts)))

(defn memoize
  "Returns a memoized version of a referentially transparent function. The
  memoized version of the function keeps a cache of the mapping from arguments
  to results and, when calls with the same arguments are repeated often, has
  higher performance at the expense of higher memory use."
  [f]
  (let [mem (atom {})]
    (fn [& args]
      (if-let [v (get @mem args)]
        v
        (let [ret (apply f args)]
          (swap! mem assoc args ret)
          ret)))))

(defn trampoline
  "trampoline can be used to convert algorithms requiring mutual
  recursion without stack consumption. Calls f with supplied args, if
  any. If f returns a fn, calls that fn with no arguments, and
  continues to repeat, until the return value is not a fn, then
  returns that non-fn value. Note that if you want to return a fn as a
  final value, you must wrap it in some data structure and unpack it
  after trampoline returns."
  ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
  ([f & args]
     (trampoline #(apply f args))))

(defn rand
  "Returns a random floating point number between 0 (inclusive) and
  n (default 1) (exclusive)."
  ([] (scm* [n] (random-real)))
  ([n] (* (rand) n)))

(defn rand-int
  "Returns a random integer between 0 (inclusive) and n (exclusive)."
  [n] (scm* [n] (random-integer n)))

(defn rand-nth
  "Return a random element of the (sequential) collection. Will have
  the same performance characteristics as nth for the given
  collection."
  [coll]
  (nth coll (rand-int (count coll))))

(defn group-by
  "Returns a map of the elements of coll keyed by the result of
  f on each element. The value at each key will be a vector of the
  corresponding elements, in the order they appeared in coll."
  [f coll]
  (reduce
   (fn [ret x]
     (let [k (f x)]
       (assoc ret k (conj (get ret k []) x))))
   {} coll))

;; UUID

(deftype UUID [uuid]
  IEquiv
  (-equiv [_ other]
    (and (instance? UUID other) (identical? uuid (.-uuid other))))

  ^:deprecation-nowarn IPrintable
  (-pr-seq [_ _]
    (list (str "#uuid \"" uuid "\"")))

  IPrintWithWriter
  (-pr-writer [_ writer _]
    (-write writer (str "#uuid \"" uuid "\"")))

  IHash
  (-hash [this]
    (goog.string/hashCode (pr-str this))))

(defn make-hierarchy
  "Creates a hierarchy object for use with derive, isa? etc."
  [] {:parents {} :descendants {} :ancestors {}})

(declare global-hierarchy)

(defn ^boolean isa?
  "Returns true if (= child parent), or child is directly or indirectly derived from
  parent, either via a JavaScript type inheritance relationship or a
  relationship established via derive. h must be a hierarchy obtained
  from make-hierarchy, if not supplied defaults to the global
  hierarchy"
  ([child parent] (isa? @global-hierarchy child parent))
  ([h child parent]
     (or (= child parent)
         ;; (and (class? parent) (class? child)
         ;;    (. ^Class parent isAssignableFrom child))
         (contains? ((:ancestors h) child) parent)
         ;;(and (class? child) (some #(contains? ((:ancestors h) %) parent) (supers child)))
         (and (vector? parent) (vector? child)
              (== (count parent) (count child))
              (loop [ret true i 0]
                (if (or (not ret) (== i (count parent)))
                  ret
                  (recur (isa? h (child i) (parent i)) (inc i))))))))

(defn parents
  "Returns the immediate parents of tag, either via a JavaScript type
  inheritance relationship or a relationship established via derive. h
  must be a hierarchy obtained from make-hierarchy, if not supplied
  defaults to the global hierarchy"
  ([tag] (parents @global-hierarchy tag))
  ([h tag] (not-empty (get (:parents h) tag))))

(defn ancestors
  "Returns the immediate and indirect parents of tag, either via a JavaScript type
  inheritance relationship or a relationship established via derive. h
  must be a hierarchy obtained from make-hierarchy, if not supplied
  defaults to the global hierarchy"
  ([tag] (ancestors @global-hierarchy tag))
  ([h tag] (not-empty (get (:ancestors h) tag))))

(defn descendants
  "Returns the immediate and indirect children of tag, through a
  relationship established via derive. h must be a hierarchy obtained
  from make-hierarchy, if not supplied defaults to the global
  hierarchy. Note: does not work on JavaScript type inheritance
  relationships."
  ([tag] (descendants @global-hierarchy tag))
  ([h tag] (not-empty (get (:descendants h) tag))))

(defn derive
  "Establishes a parent/child relationship between parent and
  tag. Parent must be a namespace-qualified symbol or keyword and
  child can be either a namespace-qualified symbol or keyword or a
  class. h must be a hierarchy obtained from make-hierarchy, if not
  supplied defaults to, and modifies, the global hierarchy."
  ([tag parent]
   (assert (namespace parent))
   ;; (assert (or (class? tag) (and (instance? cljscm.core.Named tag) (namespace tag))))
   (swap! global-hierarchy derive tag parent) nil)
  ([h tag parent]
   (assert (not= tag parent))
   ;; (assert (or (class? tag) (instance? clojure.lang.Named tag)))
   ;; (assert (instance? clojure.lang.INamed tag))
   ;; (assert (instance? clojure.lang.INamed parent))
   (let [tp (:parents h)
         td (:descendants h)
         ta (:ancestors h)
         tf (fn [m source sources target targets]
              (reduce (fn [ret k]
                        (assoc ret k
                               (reduce conj (get targets k #{}) (cons target (targets target)))))
                      m (cons source (sources source))))]
     (or
      (when-not (contains? (tp tag) parent)
        (when (contains? (ta tag) parent)
          (throw (Error. (str tag "already has" parent "as ancestor"))))
        (when (contains? (ta parent) tag)
          (throw (Error. (str "Cyclic derivation:" parent "has" tag "as ancestor"))))
        {:parents (assoc (:parents h) tag (conj (get tp tag #{}) parent))
         :ancestors (tf (:ancestors h) tag td parent ta)
         :descendants (tf (:descendants h) parent ta tag td)})
      h))))

(defn underive
  "Removes a parent/child relationship between parent and
  tag. h must be a hierarchy obtained from make-hierarchy, if not
  supplied defaults to, and modifies, the global hierarchy."
  ([tag parent]
     ;; (alter-var-root #'global-hierarchy underive tag parent)
     (swap! global-hierarchy underive tag parent) nil)
  ([h tag parent]
    (let [parentMap (:parents h)
          childsParents (if (parentMap tag)
                          (disj (parentMap tag) parent) #{})
          newParents (if (not-empty childsParents)
                      (assoc parentMap tag childsParents)
                      (dissoc parentMap tag))
          deriv-seq (flatten (map #(cons (first %) (interpose (first %) (second %)))
                                  (seq newParents)))]
      (if (contains? (parentMap tag) parent)
        (reduce #(apply derive %1 %2) (make-hierarchy)
                (partition 2 deriv-seq))
        h))))

(defn- reset-cache
  [method-cache method-table cached-hierarchy hierarchy]
  (swap! method-cache (fn [_] (deref method-table)))
  (swap! cached-hierarchy (fn [_] (deref hierarchy))))

(defn- prefers*
  [x y prefer-table]
  (let [xprefs (@prefer-table x)]
    (or
     (when (and xprefs (xprefs y))
       true)
     (loop [ps (parents y)]
       (when (pos? (count ps))
         (when (prefers* x (first ps) prefer-table) 
           true)
         (recur (rest ps))))
     (loop [ps (parents x)]
       (when (pos? (count ps))
         (when (prefers* (first ps) y prefer-table)
           true)
         (recur (rest ps))))
     false)))

(defn- dominates
  [x y prefer-table]
  (or (prefers* x y prefer-table) (isa? x y)))

(defn- find-and-cache-best-method
  [name dispatch-val hierarchy method-table prefer-table method-cache cached-hierarchy]
  (let [best-entry (reduce (fn [be [k _ :as e]]
                             (if (isa? @hierarchy dispatch-val k)
                               (let [be2 (if (or (nil? be) (dominates k (first be) prefer-table))
                                           e
                                           be)]
                                 (when-not (dominates (first be2) k prefer-table)
                                   (throw (Error.
                                           (str "Multiple methods in multimethod '" name
                                                "' match dispatch value: " dispatch-val " -> " k
                                                " and " (first be2) ", and neither is preferred"))))
                                 be2)
                               be))
                           nil @method-table)]
    (when best-entry
      (if (= @cached-hierarchy @hierarchy)
        (do
          (swap! method-cache assoc dispatch-val (second best-entry))
          (second best-entry))
        (do
          (reset-cache method-cache method-table cached-hierarchy hierarchy)
          (find-and-cache-best-method name dispatch-val hierarchy method-table prefer-table
                                      method-cache cached-hierarchy))))))

(defprotocol IMultiFn
  (-reset [mf])
  (-add-method [mf dispatch-val method])
  (-remove-method [mf dispatch-val])
  (-prefer-method [mf dispatch-val dispatch-val-y])
  (-get-method [mf dispatch-val])
  (-methods [mf])
  (-prefers [mf])
  (-dispatch [mf args]))

(defn- do-dispatch
  [mf dispatch-fn args]
  (let [dispatch-val (apply dispatch-fn args)
        target-fn (-get-method mf dispatch-val)]
    (when-not target-fn
      (throw (Error. (str "No method in multimethod '" (.-name mf) "' for dispatch value: " dispatch-val))))
    (apply target-fn args)))

(deftype MultiFn [name dispatch-fn default-dispatch-val hierarchy
                  method-table prefer-table method-cache cached-hierarchy]
  IMultiFn
  (-reset [mf]
    (swap! method-table (fn [mf] {}))
    (swap! method-cache (fn [mf] {}))
    (swap! prefer-table (fn [mf] {}))
    (swap! cached-hierarchy (fn [mf] nil))
    mf)

  (-add-method [mf dispatch-val method]
    (swap! method-table assoc dispatch-val method)
    (reset-cache method-cache method-table cached-hierarchy hierarchy)
    mf)

  (-remove-method [mf dispatch-val]
    (swap! method-table dissoc dispatch-val)
    (reset-cache method-cache method-table cached-hierarchy hierarchy)
    mf)

  (-get-method [mf dispatch-val]
    (when-not (= @cached-hierarchy @hierarchy)
      (reset-cache method-cache method-table cached-hierarchy hierarchy))
    (if-let [target-fn (@method-cache dispatch-val)]
      target-fn
      (if-let [target-fn (find-and-cache-best-method name dispatch-val hierarchy method-table
                                                     prefer-table method-cache cached-hierarchy)]
        target-fn
        (@method-table default-dispatch-val))))

  (-prefer-method [mf dispatch-val-x dispatch-val-y]
    (when (prefers* dispatch-val-x dispatch-val-y prefer-table)
      (throw (Error. (str "Preference conflict in multimethod '" name "': " dispatch-val-y
                   " is already preferred to " dispatch-val-x))))
    (swap! prefer-table
           (fn [old]
             (assoc old dispatch-val-x
                    (conj (get old dispatch-val-x #{})
                          dispatch-val-y))))
    (reset-cache method-cache method-table cached-hierarchy hierarchy))

  (-methods [mf] @method-table)
  (-prefers [mf] @prefer-table)

  (-dispatch [mf args] (do-dispatch mf dispatch-fn args))

  IHash
  (-hash [this] (scm-equal?-hash this))

  IEquiv
  (-equiv [s o] (scm* [s o] (equal? s o)))

  IFn
  (-invoke [coll args] (-dispatch coll args)))

(def PersistentVector-EMPTY_NODE (pv-fresh-node nil))
(def PersistentVector-EMPTY (PersistentVector. nil 0 5 PersistentVector-EMPTY_NODE (array) 0))
(def PersistentArrayMap-EMPTY (PersistentArrayMap. nil 0 (make-array 0) nil))
(def PersistentHashMap-EMPTY (PersistentHashMap. nil 0 nil false nil 0))
(def PersistentTreeMap-EMPTY (PersistentTreeMap. compare nil 0 nil 0))
(def PersistentTreeSet-EMPTY (PersistentTreeSet. nil (sorted-map) 0))
(def PersistentHashSet-EMPTY (PersistentHashSet. nil (hash-map) 0))

(def
  ^{:private true}
  global-hierarchy (atom (make-hierarchy)))

#_(set! cljscm.core.MultiFn.prototype.call
      (fn [_ & args]
        (this-as self
          (-dispatch self args))))

#_(set! cljscm.core.MultiFn.prototype.apply
      (fn [_ args]
        (this-as self
          (-dispatch self args))))

(defn remove-all-methods
  "Removes all of the methods of multimethod."
 [multifn]
 (-reset multifn))

(defn remove-method
  "Removes the method of multimethod associated with dispatch-value."
 [multifn dispatch-val]
 (-remove-method multifn dispatch-val))

(defn prefer-method
  "Causes the multimethod to prefer matches of dispatch-val-x over dispatch-val-y
   when there is a conflict"
  [multifn dispatch-val-x dispatch-val-y]
  (-prefer-method multifn dispatch-val-x dispatch-val-y))

(defn methods
  "Given a multimethod, returns a map of dispatch values -> dispatch fns"
  [multifn] (-methods multifn))

(defn get-method
  "Given a multimethod and a dispatch value, returns the dispatch fn
  that would apply to that value, or nil if none apply and no default"
  [multifn dispatch-val] (-get-method multifn dispatch-val))

(defn prefers
  "Given a multimethod, returns a map of preferred value -> set of other values"
  [multifn] (-prefers multifn))

;;; ExceptionInfo

(deftype ExceptionInfo [message data cause])

;;; ExceptionInfo is a special case, do not emulate this
#_(set! cljscm.core.ExceptionInfo/prototype (js/Error.))
#_(set! (.-constructor cljscm.core.ExceptionInfo/prototype) ExceptionInfo)

(defn ex-info
  "Alpha - subject to change.
  Create an instance of ExceptionInfo, an Error type that carries a
  map of additional data."
  ([msg map]
     (ExceptionInfo. msg map nil))
  ([msg map cause]
     (ExceptionInfo. msg map cause)))

(defn ex-data
  "Alpha - subject to change.
  Returns exception data (a map) if ex is an ExceptionInfo.
  Otherwise returns nil."
  [ex]
  (when (instance? ExceptionInfo ex)
    (.-data ex)))

(defn ex-message
  "Alpha - subject to change.
  Returns the message attached to the given Error / ExceptionInfo object.
  For non-Errors returns nil."
  [ex]
  (when (instance? js/Error ex)
    (.-message ex)))

(defn ex-cause
  "Alpha - subject to change.
  Returns exception cause (an Error / ExceptionInfo) if ex is an
  ExceptionInfo.
  Otherwise returns nil."
  [ex]
  (when (instance? ExceptionInfo ex)
    (.-cause ex)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; namespaces ;;;;;;;;;;;;

;runtime analog to cljscm.selfanalyzer/namespaces
(def namespaces (atom '{cljscm.core {:name cljscm.core}
                        cljscm.user {:name cljscm.user}}))

(defn get-namespaces [] cljscm.core/namespaces)

(def ^:dynamic *ns* 'cljscm.user)

(def ^:dynamic *assert* true)

(def
  ^{:doc "Each runtime environment provides a diffenent way to print output.
  Whatever function *print-fn* is bound to will be passed any
  Strings which should be printed." :dynamic true}
  *print-fn*
  (fn [s]
    (scm* [s] (display s))))

(defn string-print [x]
  (*print-fn* x)
  nil)

(def
  ^{:doc "vector of paths searched for lib loading" :dynamic true}
  *classpath* ["src/cljscm" "src" "." "src/clj"])

(defn find-file
  ([filename] (find-file filename *classpath*))
  ([filename classpath]
     (->> classpath
          (map #(str % "/" filename))
          (filter #((cljscm.core/scm* {} file-exists?) %))
          (first))))

(defn alias*
  [alias lib]
  (swap! namespaces #(assoc-in % [*ns* :requires alias] lib))
  alias)

(defn load-scm
  "loads a scheme file from classpath."
  [filename]
  (let [filename (find-file filename)]
    (when filename
      ((scm* {} load) filename))))

(defn require
  "TODO: load .cljscm and .o (right now just loads AOT compiled .scm)"
  [spec]
  (letfn [(load-once [ns reload?]
            (when (or reload? (not (get @namespaces ns)))
              (println "loading" ns)
              (load-scm (apply str (concat (replace {\. \/} (str ns)) ".scm")))))]
    (if (and (coll? spec))
      (do (binding [*ns* (first spec)]
            (load-once (first spec) (some #{:reload} spec)))
          (when (= :as (second spec))
            (alias* (nth spec 2) (first spec))))
      (binding [*ns* spec] (load-once spec false)))))

(defn special-symbol? [s]
  (if ('#{if case def fn* do let* loop* letfn* throw try* catch finally recur new set! ns deftype* defrecord* . extend scm-str* scm* & quote in-ns require} s)
    true
    false))

(defn ns-resolve
  ([ns sym]
     (ns-resolve ns {:ns (get @namespaces ns) :context :statement :locals {}} sym))
  ([ns env sym]
     (cljscm.selfanalyzer/resolve-var env sym nil))) ;TODO split out symbol resolution code from selfanalyzer

(defn resolve
  ([sym] (ns-resolve *ns* sym))
  ([env sym] (ns-resolve *ns* env sym)))

(defn scm-form-sanitize
  "Converts all vector real scheme vectors and all colls to scheme
  lists, possibly annotated with source code info"
  [form source-loc?]
  (let [san (cond
              (scm* [form] (and (pair? form) (not (list? form))))
              , ((scm* {} cons)
                 (scm-form-sanitize (scm* [form] (car form)) source-loc?)
                 (scm-form-sanitize (scm* [form] (cdr form)) source-loc?))
              (vector? form) (apply array (map #(scm-form-sanitize % source-loc?) form))
              (coll? form) (pair (map #(scm-form-sanitize % source-loc?) form))
              (symbol? form) (pair-item form)
              :else form)]
    (if (and source-loc?
             (some #{:line :column :file} (keys (meta form))))
      (let [pos (+ (dec (or (:line (meta form)) 1))
                   (* (or (:column (meta form)) 0) 65536))]
        (array
         (scm-source-marker)
         san
         (or (:file (meta form)) "(no source)")
         pos))
      san)))
