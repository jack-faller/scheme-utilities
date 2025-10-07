(define-library (utilities)
  (export identity ++ --  where where* whererec whererec* while until
          set-to-values! set-to-values!*
          define-singleton
          length= length< length<= length> length>=)
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
    (setter-impl (name ...) (pairs ... (first name*)) values)))
  (set-to-values!*
   ((_ names ... values) (setter-impl* (names ...) () values)))
  (setter-impl*
   ((_ () ((name name*) ...) values)
    (let-values (((out name* ...) values)) (set! name name*) ... out))
   ((_ (first name ...) (pairs ...) values)
    (setter-impl* (name ...) (pairs ... (first name*)) values))))

(define-syntax* define-singleton
  ((_ type name test?)
   (begin
     (define-record-type type (make-singleton) ignore)
     (define name (make-singleton))
     (define (test? object) (eq? object name)))))

(define (nth-cdr list n)
  (cond
   ((zero? n) list)
   ((null? list) #f)
   (else (nth-cdr (cdr list) (-- n)))))

(define (length= list length)
  (null? (nth-cdr list length)))
(define (length>= list length)
  (nth-cdr list length))
(define (length< list length)
  (not (length>= list length)))
(define (length<= list length)
  (length< list (++ length)))
(define (length> list length)
  (length>= list (++ length)))
