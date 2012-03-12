## clojure-scheme ##

Pervasive tail-call optimization anyone?

A few cosmetic changes to the lovely ClojureScript compiler and we are producing output suitable for compilation to C via [Gambit Scheme](http://dynamo.iro.umontreal.ca/~gambit/wiki/index.php/Main_Page)!

Protocols and Types are implemented via the [Meroon](http://dynamo.iro.umontreal.ca/~gambit/wiki/index.php/Dumping_Grounds) CLOS-like object system. 

Compilation to *.scm currently needs to be done via the host JVM Clojure REPL. This output can be manually compiled to object code via Gambit's gsc compiler. Some manual massaging of paths and load directories needs to be done at the moment.

## Performance ##

Gambit seems to be a promising compile target for Clojure:

				(scm* [] (load "_meroon") (load "core.o1"))
				(ns bench)
				(defn fib [n]
					 (if  (or (identical? n 0) (identical? n 1))
							 1
							(+  (fib (dec n) )  (fib (- n 2)))))
				(prn "fib 36:" (fib 36))

... in the Clojure REPL ...

				cljs.compiler> (time (fib 36))
				"Elapsed time: 1138.814 msecs"
				24157817
				cljs.compiler> (compile-file "bench.cljs")

... in the shell ...

				$ gsc -exe bench.scm
				$ time ./bench
				[ Meroon V3 Paques2001+1 $Revision: 1.2 $ ]
				fib 36: 24157817
				real	0m0.775s
				user	0m0.737s
				sys	0m0.009s
