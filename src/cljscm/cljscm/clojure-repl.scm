(include "source-at.scm")

(##include "~~lib/gambit#.scm")
(##include "~~lib/_gambit#.scm")

(define (wrap-code path line col code)
	(##make-source
	 code
	 (##make-locat
		(##path->container path)
		(##make-filepos (- line 1) (- col 1) 0))))

(define-type-of-repl-channel-ports clojure-repl-channel-ports
	pushback-reader)

(define (clojure-repl-write-results channel results)
  (let ((output-port (macro-repl-channel-output-port channel)))
    (##for-each
     (lambda (obj)
       (cljscm.core/prn obj))
     results)))

;a Clojure macro.
(define (cljscm.core/repl-command &form &env sym)
	(list 'scm* (vector) (list 'unquote sym)))
(cljscm.core/swap! (cljscm.core/get-namespaces) cljscm.core/assoc-in (cljscm.core/PersistentVector-fromArray (vector (quote cljscm.core) defs: (quote repl-command) macro:) #t) #t)

(define (make-clojure-repl-channel old-channel)
  (make-clojure-repl-channel-ports

   (##make-mutex 'channel-arbiter)
   (macro-current-thread)
   (macro-repl-channel-input-port old-channel)
   (macro-repl-channel-output-port old-channel)
   (##make-empty-repl-result-history)

   ##repl-channel-ports-read-command
   clojure-repl-write-results
   ##repl-channel-ports-display-monoline-message
   ##repl-channel-ports-display-multiline-message
   ##repl-channel-ports-display-continuation
   ##repl-channel-ports-pinpoint-continuation
   ##repl-channel-ports-really-exit?
   ##repl-channel-ports-newline

   (let ((nonproc-installed #f)
				 (old-read-expr (macro-repl-channel-ports-read-expr old-channel)))
		 (lambda (channel)												;read-expr
			 (if (not nonproc-installed)
					 (begin (let ((old-handler (current-exception-handler)))
										(current-exception-handler 
										 (lambda (e) 
											 (if (nonprocedure-operator-exception? e) 
													 (let  ((oper (nonprocedure-operator-exception-operator e))
																	(args (nonprocedure-operator-exception-arguments e)))
														 (polymorphic-invoke oper args))
													 (old-handler e)))))
									(set! nonproc-installed #t)))
			 (parameterize
				((cljscm.selfcompiler/*emit-source-loc?* #t))
				(let* ((reader (clojure-repl-channel-ports-pushback-reader channel))
							 (first-char (peek-char (cljscm.reader/PortPushbackReader-port reader))))
					(if (equal? #\, first-char)
							(old-read-expr channel)
							(let* ((result (cljscm.selfcompiler/emit
															(cljscm.selfanalyzer/analyze
															 (cljscm.selfanalyzer/empty-env)
															 (cljscm.reader/read reader #t #!void #f))))
										 (sanitized (cljscm.core/scm-form-sanitize result #t))
										 (output-port (macro-repl-channel-output-port channel)))
								(display "old char: ")
								(display first-char)
								(##output-port-column-set! output-port 1)
								(let ((ret (if (or (list? sanitized) (vector? sanitized))
															 (##sourcify-deep sanitized (wrap-code "(repl)" 1 1 sanitized))
															 (wrap-code "(repl)" 1 1 sanitized))))
									(display ret)
									(display "\n")
									ret)))))))

	 (cljscm.reader/port-push-back-reader (macro-repl-channel-input-port old-channel))))

(define (clone-repl-channel channel)
  (make-clojure-repl-channel-ports

   (##make-mutex 'channel-arbiter)
   (macro-current-thread)
   (macro-repl-channel-input-port channel)
   (macro-repl-channel-output-port channel)
   (##make-empty-repl-result-history)

   ##repl-channel-ports-read-command
   ##repl-channel-ports-write-results
   ##repl-channel-ports-display-monoline-message
   ##repl-channel-ports-display-multiline-message
   ##repl-channel-ports-display-continuation
   ##repl-channel-ports-pinpoint-continuation
   ##repl-channel-ports-really-exit?
   ##repl-channel-ports-newline

	 (macro-repl-channel-ports-read-expr channel)

	 (cljscm.reader/port-push-back-reader (macro-repl-channel-input-port channel))))

(define (clone2-repl-channel channel)
  (make-clojure-repl-channel-ports

   (##make-mutex 'channel-arbiter)
   (macro-current-thread)
   (macro-repl-channel-input-port channel)
   (macro-repl-channel-output-port channel)
   (##make-empty-repl-result-history)

   ##repl-channel-ports-read-command
   ##repl-channel-ports-write-results
   ##repl-channel-ports-display-monoline-message
   ##repl-channel-ports-display-multiline-message
   ##repl-channel-ports-display-continuation
   ##repl-channel-ports-pinpoint-continuation
   ##repl-channel-ports-really-exit?
   ##repl-channel-ports-newline

	 (let ((oldfn (macro-repl-channel-ports-read-expr channel)))
		 (lambda (channel)
			 (let ((result
              (let ((input-port (macro-repl-channel-input-port channel)))
                (##read-expr-from-port input-port))))
         (let ((output-port (macro-repl-channel-output-port channel)))
           (##output-port-column-set! output-port 1))
				 (display result)
				 (display "\n")
         result)))

	 (cljscm.reader/port-push-back-reader (macro-repl-channel-input-port channel))))

;(set! ##thread-make-repl-channel (lambda (thread) (make-clojure-repl-channel ##stdin-port ##stdout-port)))
(define (install-clojure-repl)
	(let* ((old-channel (macro-thread-repl-channel (macro-current-thread)))
				 (input (macro-repl-channel-input-port old-channel))
				 (output (macro-repl-channel-output-port old-channel)))
		(macro-thread-repl-channel-set! 
		 (macro-current-thread)
		 (make-clojure-repl-channel old-channel))))

'(define rc (let* ((old-channel (macro-thread-repl-channel (macro-current-thread)))
									 (input (macro-repl-channel-input-port old-channel))
									 (output (macro-repl-channel-output-port old-channel)))
							(make-clojure-repl-channel input output)))


(define (install-clone2-repl)
	(let* ((old-channel (macro-thread-repl-channel (macro-current-thread))))
		(macro-thread-repl-channel-set! 
		 (macro-current-thread)
		 (clone2-repl-channel old-channel))))
