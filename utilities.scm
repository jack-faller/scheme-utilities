(define-library (utilities)
  (export identity ++ --  where where* whererec whererec* set-to-values! while until
		  <null> make-null-singleton null-singleton? define-null)
  (import (scheme base) (utilities syntax)))

(define (identity x) x)
(define (++ x) (+ x 1))
(define (-- x) (- x 1))

(syntax-map
 (syntax-rule
  ::: (_ macro-name let-type)
  (define-syntax* macro-name
	((_ result (name value) ...) (let-type ((name value) ...) result))))
 (where let) (where* let*) (whererec letrec) (whererec* letrec*))


(define-syntax*
  (while ((_ test body ...) (let loop () (when test body ... (loop)))))
  (until ((_ test body ...) (let loop () (unless test body ... (loop))))))

(define-syntax*
  (set-to-values!
   ((_ names ... values) (setter-impl (names ...) () values)))
  (setter-impl
   ((_ () ((name name*) ...) values)
	(let-values (((name* ...) values)) (set! name name*) ...))
   ((_ (first name ...) (pairs ...) values)
	(setter-impl (name ...) (pairs ... (first name*)) values))))

(define-record-type <null>
  (make-null-singleton origin) null-singleton?
  (origin null-singleton-origin))
(define-syntax* define-null
  ((_ origin name test?)
   (begin
	 (define name (make-null-singleton 'origin))
	 (define (test? object) (eqv? object name)))))
