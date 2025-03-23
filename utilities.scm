(define-library (utilities)
  (export ++ --  where where* whererec whererec* set-to-values!)
  (import (scheme base) (utilities syntax)))

(define (++ x) (+ x 1))
(define (-- x) (- x 1))

(syntax-map
 (syntax-rule
  ::: (_ macro-name let-type)
  (define-syntax* macro-name
	((_ result (name value) ...) (let-type ((name value) ...) result))))
 (where let) (where* let*) (whererec letrec) (whererec* letrec*))

(define-syntax*
  (set-to-values!
   ((_ names ... values) (setter-impl (names ...) () values)))
  (setter-impl
   ((_ () ((name name*) ...) values)
	(let-values (((name* ...) values)) (set! name name*) ...))
   ((_ (first name ...) (pairs ...) values)
	(setter-impl (name ...) (pairs ... (first name*)) values))))
