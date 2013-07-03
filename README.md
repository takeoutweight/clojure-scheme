## clojure-scheme ##

Pervasive tail-call optimization anyone?

A few cosmetic changes to the lovely ClojureScript compiler and we are producing output suitable for compilation to C via [Gambit Scheme](http://dynamo.iro.umontreal.ca/~gambit/wiki/index.php/Main_Page)!

Clojure/West 2013 hosted a talk introducing clojure-scheme. The [slides are online](http://www.iro.umontreal.ca/~gambit/Sorenson-Clojure-to-Native-via-Scheme.pdf) and the corresponding video will be made available this August.

## Getting Started ##

The fastest way to get started is to try the self-hosted clojure-scheme REPL:

1. Install Gambit Scheme [from source](https://github.com/feeley/gambit/blob/master/INSTALL.txt) or from an [OSX/Win32 installer](http://dynamo.iro.umontreal.ca/wiki/index.php/Main_Page).
2. Put `gsc` on your PATH (`make install` puts gsc in /usr/local/Gambit-C/bin by default).
3. Install [Leiningen](https://github.com/technomancy/leiningen).
4. `git clone https://github.com/takeoutweight/clojure-scheme.git`
5. In the root clojure-scheme directory, run `lein install`
6. In the samples/repl directory, run `lein run` to build the self-hosted REPL. (src/build.clj demonstrates how to compile .clj files to scheme code).
7. run `sh run-clojure-repl` to launch the REPL, and enter `(install-clojure-repl)` to switch from Scheme mode to Clojure mode.

## Performance ##

Gambit seems to be a promising compile target for Clojure:

				(scm* [] (load "core"))
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
				fib 36: 24157817
				real	0m0.775s
				user	0m0.737s
				sys	0m0.009s
