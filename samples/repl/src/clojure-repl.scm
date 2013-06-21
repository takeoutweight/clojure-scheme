(##include "~~lib/gambit#.scm")
(##include "~~lib/_gambit#.scm")
(include "../scm/cljscm/source-at.scm")
(load "scm/cljscm/core.scm")
(cljscm.core/require '(cljscm.analyzer as: ana))
(load "scm/cljscm/core_macros.scm")
(cljscm.core/require  '(cljscm.reader))
(cljscm.core/require  '(clojure.walk))
(cljscm.core/require '(cljscm.compiler as: sc))

(define (wrap-code path line col code)
  (##make-source
   code
   (##make-locat
    (##path->container path)
    (##make-filepos (- line 1) (- col 1) 0))))

(define (interp-procedure-locals cte) ;see ##display-rte
	(define locals (list))
  (let loop1 ((c cte))
    (cond ((##cte-top? c))
          ((##cte-frame? c)
           (let loop2 ((vars (##cte-frame-vars c)))
             (if (##pair? vars)
                 (let ((var (##car vars)))
                   (if (##not (##hidden-local-var? var))
                       (set! locals (cons var locals)))
                   (loop2 (##cdr vars)))
                 (loop1 (##cte-parent-cte c)))))
          (else
           (loop1 (##cte-parent-cte c)))))
	locals)

;##continuation-locals is used per-continuation, and only for non-interp procs.
(define (continuation-locals cont)
	(define locals (list))
	(let loop ((cont (##continuation-first-frame cont #t)))
		(and cont
				 (if (##interp-continuation? cont)
						 (set! locals (append (interp-procedure-locals (macro-code-cte (##interp-continuation-code cont))) locals))
						 (let ((loc-pairs (##continuation-locals cont)))
							 (and loc-pairs (set! locals (append (map car loc-pairs) locals)))))
																				;(loop (##continuation-next-frame cont #t)) ;FIXME This doubles locals. We don't need to walk them?
				 ))
	locals)

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

(define (clojure-repl-context-prompt repl-context)

  (define (read-command)
    (let* ((channel (##thread-repl-channel-get! (macro-current-thread))) (src
           ((macro-repl-channel-read-command channel)
						channel
						repl-context))) ;can't go through ##repl-channel-read-command as we've changed arity
      (cond ((##eof-object? src)
             src)
            (else
             (let ((code (##source-code src)))
               (if (and (##pair? code)
                        (##eq? (##source-code (##car code)) 'six.prefix))
                   (let ((rest (##cdr code)))
                     (if (and (##pair? rest)
                              (##null? (##cdr rest)))
                         (##car rest)
                         src))
                   src))))))

  (##step-off) ;; turn off single-stepping

  (##repl-context-command repl-context (read-command)))

(define (make-clojure-repl-channel old-channel)
  (make-clojure-repl-channel-ports

   (##make-mutex 'channel-arbiter)
   (macro-current-thread)
   (macro-repl-channel-input-port old-channel)
   (macro-repl-channel-output-port old-channel)
   (##make-empty-repl-result-history)

	 (lambda (channel repl-context) ;repl-channel-read-command (whole context instead of just level & depth)

		 (define prompt "> ")

		 (let ((level (macro-repl-context-level repl-context))
					 (depth (macro-repl-context-depth repl-context))
					 (output-port (macro-repl-channel-output-port channel)))
			 (if (##fixnum.< 0 level)
					 (##write level output-port))
			 (if (##fixnum.< 0 depth)
					 (begin
						 (##write-string "\\" output-port)
						 (##write depth output-port)))
			 (##write-string prompt output-port)
			 (##force-output output-port))
		 ((macro-repl-channel-ports-read-expr channel) channel repl-context))
   clojure-repl-write-results
   ##repl-channel-ports-display-monoline-message
   ##repl-channel-ports-display-multiline-message
   ##repl-channel-ports-display-continuation
   ##repl-channel-ports-pinpoint-continuation
   ##repl-channel-ports-really-exit?
   ##repl-channel-ports-newline

   (let ((installed-handler #f)
         (old-read-expr (macro-repl-channel-ports-read-expr old-channel)))
     (lambda (channel repl-context)					;read-expr
       (let ((cur-handler (current-exception-handler)))
         (if (not (eq? cur-handler installed-handler))
             (begin
                                        ;(display "installing new handler\n")
               (current-exception-handler
                (lambda (e)
                  (if (nonprocedure-operator-exception? e)
                      (let  ((oper (nonprocedure-operator-exception-operator e))
                             (args (nonprocedure-operator-exception-arguments e)))
                        (polymorphic-invoke oper args))
                      (cur-handler e))))
               (set! installed-handler (current-exception-handler)))))
       (parameterize
        ((cljscm.compiler/*emit-source-loc?* #t))
        (let* ((reader (clojure-repl-channel-ports-pushback-reader channel))
               (port (cljscm.reader/PortPushbackReader-port reader))
               (first-char (let loop ((pk-char (peek-char port)))
                             (if (equal? #\newline pk-char)
                                 (begin (read-char port)
                                        (loop (peek-char port)))
                                 pk-char))))
          (if (equal? #\, first-char)
              (old-read-expr channel)
              (let* ((cont (macro-repl-context-cont repl-context))
										 (locals (continuation-locals cont))
										 (result (cljscm.compiler/emit
                              (cljscm.analyzer/analyze
                               (apply cljscm.analyzer/empty-env locals)
                               (cljscm.reader/read reader #t #!void #f))))
                     (sanitized (cljscm.core/scm-form-sanitize result #t))
                     (output-port (macro-repl-channel-output-port channel)))
                (##output-port-column-set! output-port 1)
                (let ((ret (if (or (list? sanitized) (vector? sanitized))
                               (##sourcify-deep sanitized (wrap-code "(repl)" 1 1 sanitized))
                               (wrap-code "(repl)" 1 1 sanitized))))
                                        ;(display ret)
                                        ;(display "\n")
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
         ;(display result)
         (write (current-exception-handler))
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
     (make-clojure-repl-channel old-channel))
		(set! ##repl-context-prompt clojure-repl-context-prompt)))

'(define rc (let* ((old-channel (macro-thread-repl-channel (macro-current-thread)))
                   (input (macro-repl-channel-input-port old-channel))
                   (output (macro-repl-channel-output-port old-channel)))
              (make-clojure-repl-channel input output)))


(define (install-clone2-repl)
  (let* ((old-channel (macro-thread-repl-channel (macro-current-thread))))
    (macro-thread-repl-channel-set!
     (macro-current-thread)
     (clone2-repl-channel old-channel))))

(display "\n")
(display "enter  (install-clojure-repl)  to switch to Clojure\n")
(display "enter ,? for more options\n")
