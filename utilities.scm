(define-library (utilities)
  (export identity ++ --  where where* whererec whererec* while until
          set-to-values! set-to-values!*
          <null> make-null-singleton null-singleton? define-null
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
   ((_ names ... values) (setter-impl (names ...) () values)))
  (setter-impl*
   ((_ () ((name name*) ...) values)
    (let-values (((first name* ...) values)) (set! name name*) ... first))
   ((_ (first name ...) (pairs ...) values)
    (setter-impl (name ...) (pairs ... (first name*)) values))))

(define-record-type <null>
  (make-null-singleton origin) null-singleton?
  (origin null-singleton-origin))
(define-syntax* define-null
  ((_ origin name test?)
   (begin
     (define name (make-null-singleton 'origin))
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
