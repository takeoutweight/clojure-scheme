(##define-syntax source-at
  (lambda (source-at-form)

    (define (unwrap x)
      (if (##source? x) (##source-code x) x))

    (define (wrap path line col code)
      (##make-source
       code
       (##make-locat
        (##path->container path)
        (##make-filepos (- line 1) (- col 1) 0))))

    (define (rewrap-list code)
      (cond ((null? code)
             code)
            ((pair? code)
             (cons (rewrap (car code))
                   (rewrap-list (cdr code))))
            (else
             (rewrap code))))

    (define (rewrap form)

      (define (return code)
        (if (##source? form)
            (##make-source code (##source-locat form))
            code))

      (let ((code (unwrap form)))
        (cond ((pair? code)
               (if (eq? (unwrap (car code)) 'source-at)

                   (rewrap (apply (lambda (_ path line col subform)
                                    (wrap (unwrap path)
                                          (unwrap line)
                                          (unwrap col)
                                          (unwrap (rewrap subform))))
                                  code))

                   (return (rewrap-list code))))

              ((vector? code)
               (return (list->vector (map rewrap (vector->list code)))))

              (else
               form))))

    (rewrap source-at-form)))
