## clojure-scm ##

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

## What is ClojureScript? ##

ClojureScript is a new compiler for [Clojure](http://clojure.org) that targets JavaScript. It is designed to emit JavaScript code which is compatible with the advanced compilation mode of the [Google Closure](http://code.google.com/closure/) optimizing compiler.

## Getting Started ##

* Read the [Quick Start](https://github.com/clojure/clojurescript/wiki/Quick-Start) guide.
* Read the [Documentation](https://github.com/clojure/clojurescript/wiki).
* Look at the [Sample Applications](https://github.com/clojure/clojurescript/tree/master/samples).

## Questions, Feedback? ##

Please point all of your questions and feedback [here](http://groups.google.com/group/clojure).

## Developers Welcome ##

ClojureScript operates under the same license as Clojure. All contributors must have a signed CA (Contributor's Agreement) and submit their patch via the appropriate channels. If you're interested in contributing to the project, please see the [contributing](http://clojure.org/contributing) page on [clojure.org](http://clojure.org).

## License ##

    Copyright (c) Rich Hickey. All rights reserved. The use and
    distribution terms for this software are covered by the Eclipse
    Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
    which can be found in the file epl-v10.html at the root of this
    distribution. By using this software in any fashion, you are
    agreeing to be bound by the terms of this license. You must
    not remove this notice, or any other, from this software.
