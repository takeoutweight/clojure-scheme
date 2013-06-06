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

   (let ((installed-handler #f)
         (old-read-expr (macro-repl-channel-ports-read-expr old-channel)))
     (lambda (channel)                        ;read-expr
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
              (let* ((result (cljscm.compiler/emit
                              (cljscm.analyzer/analyze
                               (cljscm.analyzer/empty-env)
                               (cljscm.reader/read reader #t #!void #f))))
                     (sanitized (cljscm.core/scm-form-sanitize result #t))
                     (output-port (macro-repl-channel-output-port channel)))
                                        ;(display "old char: ")
                                        ;(write first-char)
                                        ;(write (current-exception-handler))
                                        ;(display "\n")
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
