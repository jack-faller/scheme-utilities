(define-library (utilities sinks)
  (export
   sink sink? make-sink submit! finish! peek
   any? every? count
   (rename (sink:list list))
   (rename (sink:vector vector))
   (rename (sink:string string))
   greatest least
   fold reduce
   nth last
   find find-last)
  (import (scheme base)
          (scheme write)
          (scheme case-lambda)
          (utilities)
          (utilities syntax)))

(define-record-type <sink>
  (make-sink submit finish! peek)
  sink?
  (submit sink-submit set-sink-submit!)
  (finish! sink-finish!)
  (peek sink-peek))
(define (submit! sink elt) ((sink-submit sink) elt))
(define (finish! sink) ((sink-finish! sink)))
(define (peek sink) ((sink-peek sink)))

(define-syntax* sink (lambda)
  ((_ defs submit finish)
   (sink defs submit finish (error "Peeking not supported")))
  ((_ ((name value) ...) (lambda (parameter ...) body ...) finish peek)
   (let ((name value) ...)
     (make-sink
      (lambda (parameter ...)
	(set-to-values!* name ... (begin body ...)))
      (lambda () finish)
      (lambda () peek))))
  ((_ ((name value) ...) (lambda parameters body ...) finish peek)
   (let ((name value) ...)
     (make-sink
      (lambda parameters
        (set-to-values!* name ... (begin body ...)))
      (lambda () finish)
      (lambda () peek))))
  ((_ ((name value) ...) function finish peek)
   (let ((name value) ...)
     (lambda (source)
       (make-sink
	(syntax-variadic-lambda
	 () (a b c d e)
	 (syntax-rules ::: ()
	   ((_ args :::) (set-to-values!* name ... (f args :::)))
	   ((_ args ::: . rest) (set-to-values!* name ... (apply f args ::: rest)))))
	(lambda () finish)
        (lambda () peek))))))

(define (fold f seed)
  (sink ((seed seed))
    (lambda (next) (values #f (f seed next)))
    seed
    seed))

(define (reduce f default)
  (sink ((acc default) (first? #t))
    (lambda (next)
      (values #f (if first? next (f acc next)) #f))
    acc
    acc))

(define (nth n default)
  (sink ((value default) (count 0))
    (lambda (next)
      (values (>= count n) (if (= count n) next value) (+ count 1)))
    value
    value))
(define count
  (sink ((count 0))
    (lambda (ignore) (values #f (++ count)))
    count
    count))

(define (last default)
  (sink ((value default))
    (lambda (next) (values #f next))
    value
    value))

(define greatest
  (case-lambda
    ((default) (greatest > default))
    ((> default)
     (sink ((max default))
       (lambda (next)
         (values #f (if (> next max) next max)))
       max
       max))))
(define least
  (case-lambda
    ((default) (greatest > default))
    ((> default)
     (sink ((min default))
       (lambda (next)
         (values #f (if (> min next) next min)))
       min
       min))))

(define any
  (case-lambda
    (() (any? identity))
    ((f)
     (sink ((any? #f))
       (lambda (next)
	 (define any? (or any? (f next)))
	 (values any? any?))
       any?
       any?))))
(define every?
  (case-lambda
    (() (every? identity))
    ((f)
     (sink ((every? #t))
       (lambda (next)
	 (define every? (and every? (f next)))
	 (values (not every?) every?))
       every?
       every?))))

(define (find pred? default)
  (sink ((item default) (found? #f))
    (lambda (next)
      (if (or found? (pred? next))
	  (values #t item #t)
          (values #f item found?)))
    item
    item))
(define (find-last pred? default)
  (sink ((item default))
    (lambda (next) (values #f (if (pred? next) next item)))
    item
    item))

(define (sink:list)
  (sink ((head '()) (tail '()))
    (lambda (next)
      (define it (cons next '()))
      (if (null? head)
	  (values #f it it)
	  (begin (set-cdr! tail it)
		 (values #f head it))))
    head
    (list-copy head)))

(define (sink:vector)
  (sink ((length 0) (items (make-vector 16)))
    (lambda (next)
      (when (= length (vector-length items))
        (set! items (vector-resize items (* (vector-length items) 2))))
      (vector-set! items length next)
      (values #f (++ length) items))
    (if (= length (vector-length items)) items (vector-resize items length))
    (if (= length (vector-length items)) items (vector-resize items length))))

(define sink:string
  (case-lambda
    (() (string display))
    ((display)
     (define out (open-output-string))
     (make-sink
      (lambda (it)
        (display it out)
        #f)
      (lambda ()
        (define string (get-output-string out))
        (close-port out)
        string)
      (lambda () (get-output-string out))))))
