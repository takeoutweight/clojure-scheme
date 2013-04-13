(define c/*ns* (make-parameter 'c))
(define c/*macros* (make-parameter (make-table)))
(define c/*env* (make-parameter (make-table)))
(define c/*vars* (make-parameter (make-table)))
(define (c/in-ns ns)
	(or (table-ref (c/*macros*) ns #f) (table-set! (c/*macros*) ns (make-table))) 
	(or (table-ref (c/*vars*) ns #f) (table-set! (c/*vars*) ns (make-table))) 
	(c/*ns* ns))

(c/in-ns 'c)

(define c/*arg-env* (make-parameter #f))

(define c/*gensym-id* 0)
(define (c/next-id) (set! c/*gensym-id* (+ c/*gensym-id* 1)) c/*gensym-id*)
(define (c/gensym s)
	(string->symbol (string-append (symbol->string s)"__"(number->string(c/next-id)) "__auto__")))

(define-type c/Symbol s (meta equality-skip:))
;(define (c/-with-meta---c/_$Symbol s meta) (make c/Symbol (c/Symbol-s s) meta))
;(define (c/-meta---c/_$Symbol s) (c/Symbol-meta s))

(define (c/symbol? o)
	(or (c/Symbol? o) (symbol? o)))
(define (c/symbol->string o)
	(symbol->string (if (c/Symbol? o) (c/Symbol-s o) o)))
(define (c/->Symbol o)
	(if (c/Symbol? o) o (make-c/Symbol o #!void)))

(define-type c/Vector v (meta equality-skip:))
;; (define (c/vector? o) (or (vector? o) (c/Vector? o)))
;; (define (c/vector->list o) 
;; 	(cond ((vector? o) (vector->list o))
;; 				((c/Vector? o) (make-c/List (vector->list (c/Vector-v o)) (c/Vector-meta o)))))
(define-type c/Table t (meta equality-skip:))
(define-type c/TableSet t (meta equality-skip:))
(define (c/reduce* f acc lst)
  (if (null? lst)
      acc
      (c/reduce* f (f acc (car lst)) (cdr lst))))

(define (c/table-assoc t key val)
	(cond ((table? t) (let ((ret (c/into-table #f (c/flatten-table t))))
											(table-set! ret key val)
											ret))
				((c/Table? t) (let ((ret (c/into-table #f (c/flatten-table (c/Table-t t)))))
												(table-set! ret key val)
												(make-c/Table ret (c/Table-meta t))))))

(define (c/flatten-table t)
	(if t
			(c/reduce* 
			 (lambda (ret pair)
				 (cons (car pair) (cons (cdr pair) ret)))
			 '()
			 (table->list t))
			'()))

(define (c/flatten-table-set t)
	(c/reduce* 
	 (lambda (ret pair)
		 (cons (car pair) ret))
	 '()
	 (table->list (c/TableSet-t t))))

(define (c/into-table t lst)
	(let ((tb (or t (make-table))))
		(cond ((odd? (length lst))
					 (error "must add even number of items to map."))
					((null? lst) tb)
					(else (begin (table-set! tb (car lst) (cadr lst))
											 (c/into-table tb (cddr lst)))))))

(define (c/into-table-set t lst)
	(let ((tb (or t (make-c/TableSet (make-table) #f))))
		(cond ((null? lst) tb)
					(else (begin (table-set! (c/TableSet-t tb) (car lst) #t)
											 (c/into-table-set tb (cdr lst)))))))

(define-type c/List l (meta equality-skip:))
(define (c/list? o) (or (list? o) (c/List? o)))
(define (c/->list o) (if (list? o) o (c/List-l o)))
(define (c/car o) (car (c/->list o)))
(define (c/cadr o) (cadr (c/->list o)))
(define (c/cddr o) (cddr (c/->list o)))
(define (c/null? o) (null? (c/->list o)))

(define (c/namespace s)
	(let* ((lst (string->list (c/symbol->string s)))
				 (split (memq #\/ (reverse lst))))
		(if split 
				(let* ((last-slash-idx (- (length lst) (length split) -1)))
					(list->string (reverse (list-tail (reverse lst) last-slash-idx))))
				(void))))

(define (c/resolve s)
  (if (string? (c/namespace s))
			s
			(let ((reslvd (string->symbol (string-append (c/symbol->string (c/*ns*))
																									 "/" (c/symbol->string s)))))
				(if (c/Symbol? s)
						(make-c/Symbol s (c/Symbol-meta s))
						s))))

(define c/unquote 'c/unquote)
(define c/unquote-splicing 'c/unquote-splicing)

(define (c/expand-list lst gensym-env)
  (reverse (c/reduce*
						(lambda (ret item) 
							(if (and (list? item) (not (null? item)) (eqv? (car item) 'c/unquote-splicing))
									(cons (cadr item) ret)
									(cons (list 'list (c/syntax-quote* item gensym-env)) ret)))
						'()
						lst)))

;FIXME splice on vectors too, which can be syntax now.
(define (c/syntax-quote* f gensym-env)
	(let ((gensym-env (if (table? gensym-env) gensym-env (make-table))))
		(cond 
		 ((c/symbol? f) (list 'quote
													(if (eq? #\# (car (reverse (string->list (c/symbol->string f)))))
															(let ((f-gen (table-ref gensym-env f (gensym f))))
																(table-set! gensym-env f f-gen)
																f-gen)
															(case f
																((if case fn) f)
																(else (c/resolve f))))))
		 ((table? f) (list 'c/into-table #f (cons 'append (c/expand-list (c/flatten-table f) gensym-env))))
		 ((c/Table? f) (list 'make-c/Table
												 (c/syntax-quote* (c/Table-t f))
												 (list 'c/Table-meta (c/syntax-quote* (c/Table-meta f)))))
		 ((vector? f) (list 'apply 'vector (cons 'append (c/expand-list (vector->list f) gensym-env))))
		 ((c/Vector? f) (list 'make-c/Vector
													(c/syntax-quote* (c/Vector-v f))
													(list 'c/Vector-meta (c/syntax-quote* (c/Vector-meta f)))))
		 ;;TODO SET
		 ((c/List? f) (list 'make-c/List
												(c/syntax-quote* (c/List-l f))
												(list 'c/List-meta (c/syntax-quote* (c/List-meta f)))))
		 ((list? f) 
			(cond
			 ((null? f) f)
			 ((eqv? (car f) 'c/unquote) (cadr f))
			 ((eqv? (car f) 'c/unquote-splicing) (error "splice not in list"))
			 (else (cons 'append (c/expand-list f gensym-env)))))
		 ((or (number? f) (keyword? f) (string? f) (char? f)) f))))

(define (take* lst n ret)
  (if (zero? n)
			(reverse ret)
			(take* (cdr lst) (- n 1) (cons (car lst) ret))))
(define (take lst n) (take* lst n '()))

(define (macro-expand-decompile-thunk thunk+)
  (let* ((code (cddr (##decompile thunk+)))
				 (len (length code))
				 ;; strip last faked element:
				 (code (take code (- len 1))))
    (cond
     ((null? code)
      '(begin))
     ((null? (cdr code))
      (car code))
     (else
      `(begin , at code)))))

;call w/o defensive quote, seems to work on the list example?
;Basically same as below, skips the reading/pretty printing by working on decompiled forms.
(define-macro (macro-expand2 syntax)
  `(macro-expand-decompile-thunk (lambda () ,syntax 
																		(fake-form))))

;seems to work same as above? Reads from a prett-printed lambda
(define-macro (macro-expand3 mac)
  (let ((port (gensym))
        (form (gensym))
        (code (gensym)))
    `(let ((,port (open-string)))
        (pp (lambda () ,mac) ,port)
        (let* ((,form (read ,port))
               (,code (cddr ,form)))
          (cond ((null? ,code) '(begin))
                ((null? (cdr ,code)) (car ,code))
                (else `(begin ,@,code)))))))

;doesn't work: try expanding (ret3 45) into the list
(define-macro (macro-expand-bad f)
  `(eval (quote ,f)))

(define (test f) `(list 1 2 ,f))

;proper macro-expansion should return the form (list 1 2 3)
(define-macro (ret3 f)
  (test f))

;in-ns as IN_NAMESPACE symbol set up in RT.java calls inNamespace, Namespace.findOrCreate(name), then CURRENT_NS.set(ns)

;---------- WRITER ------------

(define (c/writer we obj)
	(letrec ((space-sep (lambda (rst) (or (null? rst)
																	 (begin (##wr we (car rst))
																					(and (> (length rst) 1) (##wr-str we " "))
																					(space-sep (cdr rst)))))))
		(cond ((c/Symbol? obj) (##wr we (c/Symbol-s obj)))
					;((char? obj) (##wr we (substring (object->string obj) 1 3))) Think object->string calls the writer - infinite loop.
					((vector? obj) (##wr-str we "[") 
					 (space-sep (vector->list obj))
					 (##wr-str we "]"))
					((keyword? obj) (##wr-str we (string-append ":" (keyword->string obj))))
					((table? obj) (##wr-str we "{")
					 (space-sep (c/flatten-table obj))
					 (##wr-str we "}"))
					((c/TableSet? obj) (##wr-str we "#{")
					 (space-sep (c/flatten-table-set obj))
					 (##wr-str we "}"))
					(else (##default-wr we obj)))))
(set! ##wr c/writer)

;---------- READER ------------

;(##include "~~lib/gambit#.scm")
;(##include "~~lib/_gambit#.scm")

(##define-macro (macro-peek-next-char-or-eof re) ;; possibly returns EOF
  `(macro-peek-char (macro-readenv-port ,re)))

(##define-macro (macro-read-next-char-or-eof re) ;; possibly returns EOF
  `(macro-read-char (macro-readenv-port ,re)))

(define (bard-make-readtable)
  (let ((rt (##make-standard-readtable)))

    (define (set-number/keyword/symbol-starter first last)
      (let loop ((i (char->integer first)))
        (if (<= i (char->integer last))
            (begin
              (##readtable-char-class-set!
               rt
               (integer->char i)
               #f
               bard-read-number/keyword/symbol)
              (loop (+ i 1))))))

    (set-number/keyword/symbol-starter #\a #\z)
    (set-number/keyword/symbol-starter #\A #\Z)
    (set-number/keyword/symbol-starter #\0 #\9)
    (set-number/keyword/symbol-starter #\+ #\+)
    (set-number/keyword/symbol-starter #\- #\-)

    (macro-readtable-keywords-allowed?-set! rt 'prefix)

;    (##readtable-char-class-set! rt #\' #t bard-read-quote)
    (##readtable-char-class-set! rt #\` #t c/read-syntax-quote)
    (##readtable-char-class-set! rt #\~ #t c/read-unquote-or-splice)
    (##readtable-char-class-set! rt #\^ #t c/read-meta)
    (##readtable-char-class-set! rt #\\ #t read-char)
    (##readtable-char-class-set! rt #\. #f bard-read-number/keyword/symbol)
;    (##readtable-char-class-set! rt #\& #t ##read-dot)
    (##readtable-char-class-set! rt #\[ #t c/read-vector)
    (##readtable-char-class-set! rt #\{ #t c/read-map)
    (##readtable-char-class-set! rt #\% #f c/read-arg)

		(##readtable-char-sharp-handler-set! rt #\( c/read-fn)
		(##readtable-char-sharp-handler-set! rt #\( c/read-fn)
		(##readtable-char-sharp-handler-set! rt #\& c/read-dot)

    rt))

(define (c/read-syntax-quote re c)
	(let ((start-pos (##readenv-current-filepos re)))
		(macro-read-next-char-or-eof re) ;; skip `
    (macro-readenv-filepos-set! re start-pos) ;; set pos to start of datum
		(let ((r (##read-datum-or-eof re)))
			(macro-readenv-wrap re (c/syntax-quote* r #f)))))

(define (c/read-unquote-or-splice re c)
	(let ((start-pos (##readenv-current-filepos re)))
		(macro-read-next-char-or-eof re) ;; skip ~
		(let ((c (macro-peek-next-char-or-eof re)))
			(if (eqv? c #\@)
					(begin 
						(macro-read-next-char-or-eof re) ;; skip @
						(macro-readenv-filepos-set! re start-pos) ;; set pos to start of datum
						(macro-readenv-wrap re (list 'c/unquote-splicing (##read-datum-or-eof re))))
					(begin
						(macro-readenv-filepos-set! re start-pos) ;; set pos to start of datum
						(macro-readenv-wrap re (list 'c/unquote (##read-datum-or-eof re)))))))) 

(define (c/read-meta re c)
	(let ((start-pos (##readenv-current-filepos re)))
		(macro-read-next-char-or-eof re)					;; skip ^
    (macro-readenv-filepos-set! re start-pos) ;; set pos to start of datum
		(let* ((m (##read-datum-or-eof re))
					 (m (cond ((or (c/symbol? m) (string? m))
										 (c/into-table #f (list tag: m)))
										((keyword? m)
										 (c/into-table #f (list m #t)))
										((table? m) m)
										(else  (error "Metadata must be Symbol,Keyword,String or Map"))))
					 (o (##read-datum-or-eof re)))
			(cond ((symbol? o) (macro-readenv-wrap re (make-c/Symbol o m)))
						((vector? o) (macro-readenv-wrap re (make-c/Vector o m)))
						((table?  o) (macro-readenv-wrap re (make-c/Table  o m)))
						((list?   o) (macro-readenv-wrap re (make-c/List   o m)))
						((c/Symbol?   o) (macro-readenv-wrap re (make-c/Symbol   (c/Symbol-s   o) (c/into-table #f (append (c/flatten-table (c/Symbol-meta   o)) (c/flatten-table m))))))
						((c/Vector?   o) (macro-readenv-wrap re (make-c/Vector   (c/Vector-v   o) (c/into-table #f (append (c/flatten-table (c/Vector-meta   o)) (c/flatten-table m))))))
						((c/Table?    o) (macro-readenv-wrap re (make-c/Table    (c/Table-t    o) (c/into-table #f (append (c/flatten-table (c/Table-meta    o)) (c/flatten-table m))))))
						((c/TableSet? o) (macro-readenv-wrap re (make-c/TableSet (c/TableSet-t o) (c/into-table #f (append (c/flatten-table (c/TableSet-meta o)) (c/flatten-table m))))))
						((c/List?     o) (macro-readenv-wrap re (make-c/List     (c/List-l     o) (c/into-table #f (append (c/flatten-table (c/List-meta     o)) (c/flatten-table m))))))
						(else (error "reader meta only allowed on symbols, vectors, lists, maps, and set syntax objects"))))))

;; (macro-read-next-char-or-eof re)
;; 	(let* ((r (##read-expr-from-port
;; 						(macro-readenv-port re)))
;; 				(e (##eval r)))
;; 		(macro-readenv-wrap re (list 'hi e)))

;(define (c/read-unquote re c))

;(define (c/read-unquote-splicing re c))

(define (c/read-vector re c)
  (let ((start-pos (##readenv-current-filepos re)))
    (macro-read-next-char-or-eof re) ;; skip #\( or #\[ or #\{ or #\<
    (macro-readenv-filepos-set! re start-pos) ;; set pos to start of datum
    (let* ((close (##closing-parenthesis-for c))
           (lst (##build-list re #t start-pos close)))
			(macro-readenv-wrap re (apply vector lst)) ))) ; 

(define (c/read-map re c)
  (let ((start-pos (##readenv-current-filepos re)))
    (macro-read-next-char-or-eof re) ;; skip #\( or #\[ or #\{ or #\<
    (macro-readenv-filepos-set! re start-pos) ;; set pos to start of datum
    (let* ((close (##closing-parenthesis-for c))
           (lst (##build-list re #t start-pos close)))
			(macro-readenv-wrap re (c/into-table #f lst)))))

(define (c/read-set re c start-pos)
  (let ((start-pos (##readenv-current-filepos re)))
    (macro-read-next-char-or-eof re) ;; skip opening {
    (macro-readenv-filepos-set! re start-pos) ;; set pos to start of datum
    (let* ((lst (##build-list re #t start-pos #\})))
			(macro-readenv-wrap re (c/into-table-set #f lst)))))

(define (c/register-arg n)
	(let ((sym (table-ref (c/*arg-env*) n #f)))
		(or sym
				(let ((sym (string->symbol 
										(string-append (if (eqv? n -1) "rest"
																			 (string-append "p" (number->string n))) 
																	 "__" (number->string (c/next-id))"#"))))
				  (table-set! (c/*arg-env*) n sym)
					sym))))

(define (c/read-fn re next start-pos)
	(if (c/*arg-env*)
			(error "Nested #()s are not allowed")
			(parameterize
			 ((c/*arg-env* (make-table)))
			 (macro-read-next-char-or-eof re)					 ;; skip ( after #
			 (macro-readenv-filepos-set! re start-pos) ;; set pos to start of datum
			 (let* ((lst (##build-list re #t start-pos #\) ))
							(high-idx (apply max (cons 0 (map car (table->list (c/*arg-env*))))))
							(rest? (table-ref (c/*arg-env*) -1 #f)))
				 (letrec ((add-arg 
									 (lambda (n ret) (if (zero? n) 
																	ret
																	(add-arg (- n 1)
																					 (cons (c/register-arg n)
																								 ret))))))
					 (let* ((args (apply vector
															 (add-arg high-idx
																				(if rest? 
																						(list '& (c/register-arg -1))
																						(list))))))
						 (macro-readenv-wrap re (list 'fn* args lst))))))))

(define (c/read-arg re c)
	(let* ((start-pos (##readenv-current-filepos re))) 
		(if (c/*arg-env*)
				(let* ((_ (macro-read-next-char-or-eof re)) ;; skip %
							 (c (macro-peek-next-char-or-eof re))
							 (d (- (char->integer c) 48))) 
					(cond ((and (number? d) (positive? d) (< d 10))
								 (let ((num (##read-datum-or-eof re))) 
									 (macro-readenv-filepos-set! re start-pos)
									 (macro-readenv-wrap re (c/register-arg num))))
								((eqv? c #\&)
								 (begin 
									 (##read-datum-or-eof re)
									 (macro-readenv-filepos-set! re start-pos)											
									 (macro-readenv-wrap re (c/register-arg -1))))
								(else (begin 
												(macro-readenv-filepos-set! re start-pos)
												(cond (else (macro-readenv-wrap re (c/register-arg 1))))))))				
				(begin
					(macro-read-next-char-or-eof re)
					(let* ((s (##read-datum-or-eof re))
								 (st (cond ((c/symbol? s) (c/symbol->string s))
													 ((number? s) (number->string s))
													 (else "")))
								 (sym (string->symbol (string-append "%" st))))
						(macro-readenv-filepos-set! re start-pos)
						(macro-readenv-wrap re sym))))))

(define (c/read-dot re next start-pos)
	(##read-dot re next))

(define (bard-read-number/keyword/symbol re c)
  (let ((start-pos (##readenv-current-filepos re)))

    (macro-read-next-char-or-eof re) ;; skip "c"

    (macro-readenv-filepos-set! re start-pos) ;; set pos to start of datum

    (let ((obj (##build-delimited-number/keyword/symbol re c #t)))
      (let ((x (assq obj bard-special-symbols)))
        (cond (x
               (macro-readenv-wrap re (cdr x)))
              ((c/symbol? obj)
               (let ((lst
                      (map string->symbol
                           (call-with-input-string
                            (c/symbol->string obj)
                            (lambda (port)
                              (read-all port
                                        (lambda (port) (read-line port #\:))))))))
                 (if (= 1 (length lst))
                     (macro-readenv-wrap re (car lst))
                     (macro-readenv-wrap
                      re
                      (map (lambda (x) (macro-readenv-wrap re x))
                           (cons 'qualified-name lst))))))
              (else
               (macro-readenv-wrap re obj)))))))

(define bard-special-symbols
  (list (cons 'undefined (if #f 123))
        (cons 'nothing   'nil)
        (cons 'true      #t)
				(cons 'nil      #!void)
        (cons 'false     #f)))

(define (bard-read-quote re c)
  (let ((start-pos (##readenv-current-filepos re)))

    (macro-read-next-char-or-eof re) ;; skip "c"

    (macro-readenv-filepos-set! re start-pos) ;; set pos to start of datum

    (macro-readenv-wrap re bard-quote)))

(define bard-quote (list 'bard-quote)) ;(lambda () 'hi)

(define (bard-read-comma re c)
  (let ((start-pos (##readenv-current-filepos re)))

    (macro-read-next-char-or-eof re) ;; skip "c"

    (macro-readenv-filepos-set! re start-pos) ;; set pos to start of datum

    (macro-readenv-wrap re bard-comma)))

(define bard-comma (list 'bard-comma))

(define (read-char re c)
	(##read-sharp-char re c (macro-readenv-filepos re)))

(define (bard-read-character re c)
  (let ((start-pos (##readenv-current-filepos re)))

    (macro-read-next-char-or-eof re) ;; skip backslash

    (macro-readenv-filepos-set! re start-pos) ;; set pos to start of datum

    (let ((c (macro-read-next-char-or-eof re))) ;; read char after backslash

      (cond ((not (char? c))
             (error "incomplete form, EOF reached"))

            ((char-alphabetic? c)
             (let ((name (##build-delimited-string re c 1)))

               (define (not-hex)
                 (let ((x (assoc name
                                 (macro-readtable-named-char-table
                                  (macro-readenv-readtable re)))))
                   (if x
                       (macro-readenv-wrap re (cdr x))
                       (if (= 1 (string-length name))
                           (macro-readenv-wrap re (string-ref name 0))
                           (error "unknown character name" name)))))

               (if (and (= 6 (string-length name))
                        (char=? (string-ref name 0) #\u)
                        (char=? (string-ref name 1) #\+))
                   (let ((n (string->number (substring name 2 6) 16)))
                     (if n
                         (macro-readenv-wrap re (integer->char n))
                         (not-hex)))
                   (not-hex))))

            (else
             (macro-readenv-wrap re c))))))

;;---------------------------------------------------------------------

;; test it
;(##current-readtable)

(define (bard-read-all-from-string str)
  (call-with-input-string
   (list init: str
         readtable: (bard-make-readtable))
   read-all))

(define (resolver exc)
	(if (unbound-global-exception? exc)
			good
			'fail))

#|
(let ((p (open-input-file (list path: "file.cljsm" readtable: (bard-make-readtable)))))
	(read-all p))
|#

;(with-exception-catcher resolver (lambda () blargle))
;(with-exception-handler resolver (lambda () blargle))

(define-macro (with-local loc . forms)
	`(parameterize ((c/*env* (c/table-assoc (c/*env*) ,loc (make-table)))) ,@forms))

(define (parse-let o env) 
	(let ((bindings (c/cadr o))
				(body (c/cddr o)))
		(letrec ((analyze-bindings
							(lambda (ana-binds env binds)
								(if (null? binds) 
										(list ana-binds env)
										(let ((next-env (c/table-assoc env (car binds) (make-table))))
											(analyze-bindings 
											 (cons (list (car binds) (analyze (cadr binds) env)) ana-binds)
											 next-env
											 (cddr binds)))))))
			(let* ((ana (analyze-bindings '() env (vector->list (c/cadr o))))
						 (binds (reverse (car ana)))
						 (next-env (cadr ana))
						 (bd (map (lambda (b) (analyze b next-env)) body)))
				`(let* ,binds ,@bd)))))

;; (define (parse-fn o env) 
;; 	(let* ((args (if (c/symbol? (c/cadr o))))
;; 				(body (c/cddr o)))
;; 		(letrec ((analyze-bindings
;; 							(lambda (ana-binds env binds)
;; 								(if (null? binds) 
;; 										(list ana-binds env)
;; 										(let ((next-env (c/table-assoc env (car binds) (make-table))))
;; 											(analyze-bindings 
;; 											 (cons (list (car binds) (analyze (cadr binds) env)) ana-binds)
;; 											 next-env
;; 											 (cddr binds)))))))
;; 			(let* ((ana (analyze-bindings '() env (vector->list (c/cadr o))))
;; 						 (binds (reverse (car ana)))
;; 						 (next-env (cadr ana))
;; 						 (bd (map (lambda (b) (analyze b next-env)) body)))
;; 				`(let* ,binds ,@bd)))))

(define (analyze o env)
	(cond 
	 ((c/list? o) (if (c/null? o) 
										o
										(case (c/car o)
											((def) (table-set! (table-ref (c/*vars*) (c/*ns*)) (c/cadr o)))
											((let*) (parse-let o env))
											((if let* fn* do) o)
											(else (let ((mac (table-ref (table-ref (c/*macros*) (c/*ns*)) (c/->Symbol (c/cadr o)) #f)))
															(if mac (analyze (apply mac o env (c/cdr o)))
																	o))))))
	 (else o)))

#|
(begin
(##include "~~lib/gambit#.scm")
(##include "~~lib/_gambit#.scm")
(include "corescratch.scm")
(define oldt (##current-readtable))
(##current-readtable (bard-make-readtable))
)
(include "~/src/c-clojure/src/cljscm/cljscm/core.scm")
(define-macro (cljscm.selfanalyzer/defmacro . rst) "defmacro")
(include "~/src/c-clojure/src/clj/cljscm/selfanalyzer.scm")
(include "~/src/c-clojure/src/cljscm/cljscm/core_macros.scm")
(include "in.clj")
(##current-readtable oldt)
(parameterize ((##current-readtable (bard-make-readtable))) (include "~/src/c-clojure/src/clj/cljscm/selfanalyzer.scm"))
(parameterize ((##current-readtable (bard-make-readtable))) (include "in.clj"))
(let ((p (open-input-file (list path: "in.clj" readtable: (bard-make-readtable))))) (read-all p))
(let ((p (open-input-file (list path: "in.clj")))) (read-all p))
(let ((p (open-input-file (list path: "~/src/c-clojure/src/clj/cljscm/selfanalyzer.scm" readtable: (bard-make-readtable))))) (read-all p))
(bard-read-all-from-string "(a & b)")
|#


#|
(begin
 (declare (standard-bindings) (extended-bindings) (block))
 (load "~/src/c-clojure/src/cljscm/cljscm/core.scm")
; (load cljscm.conditional)
)
|#
