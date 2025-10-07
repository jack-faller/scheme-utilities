(define-library (utilities sinks)
  (export
   sink sink? make-sink submit! finish!
   any? every? count
   ;; TODO: this is wrong but Guile requires these brackets here.
   (rename (sink:list list))
   (rename (sink:vector vector))
   (rename (sink:string string))
   ;; TODO: max min
   fold reduce
   nth last
   find find-last)
  (import (scheme base)
          (scheme write)
          (scheme case-lambda)
          (utilities)
          (utilities syntax)))

(define-record-type <sink>
  (make-sink submit finish!)
  sink?
  (submit sink-submit set-sink-submit!)
  (finish! sink-finish!))
(define (submit! sink elt) ((sink-submit sink) elt))
(define (finish! sink) ((sink-finish! sink)))

(define-syntax* sink (lambda)
  ((_ ((name value) ...) (lambda (parameter ...) body ...) finish)
   (let ((name value) ...)
     (make-sink
      (lambda (parameter ...)
	(set-to-values!* name ... (begin body ...)))
      (lambda () finish))))
  ((_ ((name value) ...) (lambda parameters body ...) finish)
   (let ((name value) ...)
     (make-sink
      (lambda parameters
        (set-to-values!* name ... (begin body ...)))
      (lambda () finish))))
  ((_ ((name value) ...) function finish)
   (let ((name value) ...)
     (lambda (source)
       (make-sink
	(syntax-variadic-lambda
	 () (a b c d e)
	 (syntax-rules ::: ()
	   ((_ args :::) (set-to-values!* name ... (f args :::)))
	   ((_ args ::: . rest) (set-to-values!* name ... (apply f args ::: rest)))))
	(lambda () finish))))))

(define (fold f seed)
  (sink ((seed seed))
    (lambda (next) (values #f (f seed next)))
    seed))

(define (reduce f default)
  (sink ((acc default) (first? #t))
    (lambda (next)
      (values #f (if first? next (f acc next)) #f))
    acc))

(define (nth n default)
  (sink ((value default) (count 0))
    (lambda (next)
      (values (>= count n) (if (= count n) next value) (+ count 1)))
    value))
(define count
  (sink ((count 0))
    (lambda (ignore) (values #f (++ count)))
    count))

(define (last default)
  (sink ((value default))
    (lambda (next) (values #f next))
    value))

(define any
  (case-lambda
    (() (any? identity))
    ((f)
     (sink ((any? #f))
       (lambda (next)
	 (define any? (or any? (f next)))
	 (values any? any?))
       any?))))
(define every?
  (case-lambda
    (() (every? identity))
    ((f)
     (sink ((every? #t))
       (lambda (next)
	 (define every? (and every? (f next)))
	 (values (not every?) every?))
       every?))))

(define (find pred? default)
  (sink ((item default) (found? #f))
    (lambda (next)
      (if (or found? (pred? next))
	  (values #t item #t)
          (values #f item found?)))
    item))
(define (find-last pred? default)
  (sink ((item default))
    (lambda (next) (values #f (if (pred? next) next item)))
    item))

(define (sink:list)
  (sink ((head '()) (tail '()))
    (lambda (next)
      (define it (cons next '()))
      (if (null? head)
	  (values #f it it)
	  (begin (set-cdr! tail it)
		 (values #f head it))))
    head))

(define (sink:vector)
  (define list (sink:list))
  (sink ((count 0))
    (lambda (next)
      (submit! list next)
      (values #f (++ count)))
    (let ((out (make-vector count))
	  (i 0))
      (for-each
       (lambda (x)
	 (vector-set! out i x)
	 (set! i (++ i)))
       (finish! list))
      out)))

(define sink:string
  (case-lambda
    (() (string display))
    ((display)
     (define out (open-output-string))
     (make-sink
      (lambda (it)
        (display it out)
        #f)
      (lambda () (get-output-string out))))))



















