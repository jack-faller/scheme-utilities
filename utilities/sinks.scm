(define-library (utilities sinks)
  (export
   sink
   any? every? count
   string
   ;; This doesn't work so I just have to clobber list.
   ;; (rename list-sink list)
   list
   ;; TODO: vector
   fold reduce
   nth last
   find find-last)
  (import (scheme base) (utilities) (utilities syntax)))

(define-syntax* sink (lambda)
  ((_ ((name value) ...) (lambda (parameter ...) body ...) finish)
   (let ((name value) ...)
	 (lambda (source)
	   (source (lambda (parameter ...)
				 (set-to-values! name ... (begin body ...))))
	   finish)))
  ((_ ((name value) ...) (lambda parameters body ...) finish)
   (let ((name value) ...)
	 (lambda (source)
	   (source (lambda parameters
				 (set-to-values! name ... (begin body ...))))
	   finish)))
  ((_ ((name value) ...) function finish)
   (let ((name value) ...)
	 (lambda (source)
	   (source
		(syntax-variadic-lambda
		 () (a b c d e)
		 (syntax-rules ::: ()
		   ((_ args :::) (set-to-values! name ... (f args :::)))
		   ((_ args ::: . rest) (set-to-values! name ... (apply f args ::: rest))))))
	   finish))))

(define (fold f seed)
  (sink ((seed seed))
		(lambda (next) (f seed next))
		seed))

(define (reduce f default)
  (sink ((acc default) (first? #t))
		(lambda (next)
		  (values (if first? next (f acc next)) #f))
		acc))

(define (nth n default)
  (sink ((value default) (count 0))
		(lambda (next)
		  (values (if (= count n) next value) (+ count 1)))
		value))
(define count
  (sink ((count 0))
		(lambda ignore (++ count))
		count))

(define (last default) (sink ((value defaut)) (lambda (next) next) value))

(define (any? f)
  (sink ((any? #f)) (lambda (next) (or any? (f next))) any?))
(define (every? f)
  (sink ((every? #t)) (lambda (next) (and every? (f next))) every?))

(define (string source)
  (define out (open-output-string))
  (source (lambda (c) (write-char c out)))
  (get-output-string out))

(define (find pred? default)
  (sink ((item default) (found? #f))
		(lambda (next)
		  (cond (found? (values item found?))
				((f next) (values next #t))
				(else (values item found?))))
		item))
(define (find-right pred? default)
  (sink ((item default))
		(lambda (next) (if (f next) next item))
		item))

(define (list source)
  (define head '())
  (define tail '())
  (source
   (lambda (next)
	 (if (null? head)
		 (begin
		   (set! head (cons next '()))
		   (set! tail head))
		 (begin
		   (set-cdr! tail (cons next '()))
		   (set! tail (cdr tail)))))))
