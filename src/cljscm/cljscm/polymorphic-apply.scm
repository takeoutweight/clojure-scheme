(define (##apply-global-with-procedure-check-nary gv . args)
  (##declare (not interrupts-enabled))
  (polymorphic-apply-with-procedure-check (##global-var-ref gv) args))

(define (##apply-with-procedure-check-nary oper . args)
  (##declare (not interrupts-enabled))
  (polymorphic-apply-with-procedure-check oper args))

(define (##apply-with-procedure-check oper args)
  (##declare (not interrupts-enabled))
  (if (##procedure? oper)
    (##apply oper args)
    (polymorphic-invoke oper args)))

(define (polymorphic-apply-with-procedure-check oper args)
  (##declare (not interrupts-enabled))
  (if (##procedure? oper)
    (##apply oper args)
    (polymorphic-invoke oper args)))

(define (polymorphic-invoke oper args)
  (##declare (not interrupts-enabled))
  (if (eqv? #!void oper)
			(raise "-invoke called on nil")
			(cljscm.core/-invoke oper args)))

;This seems to be necessary for interpreted code. Compiled code uses above procedures.
(let ((old-handler (current-exception-handler)))
  (current-exception-handler 
   (lambda (e) 
           (if (nonprocedure-operator-exception? e) 
             (let  ((oper (nonprocedure-operator-exception-operator e))
                    (args (nonprocedure-operator-exception-arguments e)))
               (polymorphic-invoke oper args))
             (old-handler e)))))
