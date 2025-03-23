(define-library (utilities iterators)
  (export
   ;; Primitive creation.
   iterator iterator?
   iterator-defer! iterator-replace!
   iter-null iter-null? iter-or
   ;; TODO: a version of the iterate constructor which takes a function as an argument.
   ;; TODO: (iterate-for[*!] null recur recur-next ((ivar iter) ...) ((var val) ...) body ...)
   ;; recur loops setting the variables, recur-next loops without setting the variables
   iterate iterate*
   let-next if-let-next apply-to-next apply-to-next* iter-for iter-for!

   ;; Access.
   iter-find iter-next iter-peek

   ;; Creation.
   just
   iter-list
   list->iter repeat-list->iter pairs->iter
   string->iter repeat-string->iter
   vector->iter repeat-vector->iter
   port->char-iter
   iter-forever iter-repeat
   tree->iter
   iter-range iter-iota

   ;; Manipulation.
   iter-map iter-scan iter-filter iter-remove
   iter-take iter-take-while iter-drop iter-drop-while
   iter-flatten iter-recursive-flatten
   iter-duplicate
   iter-cons
   iter-zip

   ;; TODO: nth!
   ;; TODO: (collect! sink iterator ...) <- allows multi-valued sinks
   ;; TODO: rename all functions to remove iter

   ;; Collection.
   iter-any? iter-every? iter-count
   iter->string iter->list iter->vector
   iter-for-each iter-run iter-last
   iter-fold iter-reduce)

  (import (scheme base) (scheme case-lambda) (utilities syntax)))
(define (identity x) x)

(define-record-type <iterator>
  (make-iterator next-function)
  iterator?
  (next-function iterator-next-function set-iterator-next-function!))

(define-syntax iterator
  (syntax-rules ()
	((_ () body ...)
	 (iterator ignored-1 (ignored-2) body ...))
	((_ (this) body ...)
	 (iterator ignored-1 (this) body ...))
	((_ name () body ...)
	 (iterator name (ignored-2) body ...))
	((_ recur (this) body ...)
	 (make-iterator
	  (letrec ((function (lambda (this) (let recur () body ...))))
		function)))))

(define-syntax define-syntax*
  (syntax-rules ()
	((_ (rule ...) ...) (begin (define-syntax* rule ...) ...))
	((_ name ((pat . pats) sub) ...)
	 (define-syntax name (syntax-rules () ((pat . pats) sub) ...)))
	((_ name (ident ...) (pat sub) ...)
	 (define-syntax name (syntax-rules (ident ...) (pat sub) ...)))
	;; SRFI-46 custom ellipses.
	((_ name ellip ((pat . pats) sub) ...)
	 (define-syntax name (syntax-rules ellip () ((pat . pats) sub) ...)))
	((_ name ellip (ident ...) (pat sub) ...)
	 (define-syntax name (syntax-rules ellip (ident ...) (pat sub) ...)))))
(define-syntax* syntax-rule
  ((_ pat sub) (syntax-rules () (pat sub)))
  ((_ (lit ...) pat sub) (syntax-rules (lit ...) (pat sub)))
  ((_ ellip pat sub) (syntax-rules ellip () (pat sub)))
  ((_ ellip lit pat sub) (syntax-rules ellip lit (pat sub))))

(define-syntax*
  (syntax-for-each
   ((_ rules values ...)
	(let-syntax ((macro rules))
	  (macro . values) ...)))
  (macro-map*
   ((_ (name ...) ((value ...) ...) body ...)
	(let-syntax ((body-macro (syntax-rules ignore ()
							   ((_ name ...) (begin body ...)))))
	  (body-macro value ...) ...)))
  (macro-map
   ((_ name (value ...) body ...) (macro-map* (name) ((value) ...) body ...)))
  (while ((_ test body ...) (let loop () (when test body ... (loop)))))
  (iter-or
   ((_ item) item)
   ((_ first item ...)
	(let ((i first)) (if (iter-null? i) (iter-or item ...) i))))
  (apply-to-next*-impl
   ((_ false-case f arg ... ()) (f arg ...))
   ((_ false-case f arg ... (first iterator ...))
	(let ((it (iter-next first)))
	  (if (iter-null? it)
		  (false-case)
		  (apply-to-next*-impl false-case f arg ... it (iterator ...))))))
  (apply-to-next*
   ((_ false-case f arg ... (iterator ...))
	(let ((else (lambda () false-case)))
	  (apply-to-next*-impl else f arg ... (iterator ...)))))
  (apply-to-next
   ((_ f arg ... (iterator ...))
	(apply-to-next* iter-null f arg ... (iterator ...))))
  (let-next
	  ((_ ((name val) ...) body ...)
	   (apply-to-next (lambda (name ...) body ...) (val ...))))
  (if-let-next
	  ((_ ((name val) ...) then else)
	   (apply-to-next* else (lambda (name ...) then) (val ...))))
  (setter-impl
   ((_ () ((name name-name) ...))
	(lambda (return name-name ...) (set! name name-name) ... return))
   ((_ (first name ...) (pairs ...))
	(setter-impl (name ...) (pairs ... (first name-name)))))
  ;; For use in iterate.
  (setter ((_ names ...) (setter-impl (names ...) ())))
  (iterate-impl
   ((_ let null recur ((name value) ...) body ...)
	(let ((name value) ...)
	  (iterator ()
		(call-with-values
			(lambda ()
			  (letrec ((s (setter name ...))
					   (null (lambda () (values iter-null name ...)))
					   (recur (lambda (name ...) (s #f name ...) (do-body)))
					   (do-body (lambda () body ...)))
				(do-body)))
		  (setter name ...))))))
  (iter-zip-impl
   ((_ () (names ...))
	(iterator () (apply-to-next values (names ...))))
   ((_ (it its ...) (names ...))
	(let ((name it)) (iter-zip-impl (its ...) (names ... name)))))
  (iter-zip
   ((_ its ...)
	(iter-zip-impl (its ...) ())))
  (iter-for ((_ ((name iter) ...) body ...)
			 (let ((name iter) ...)
			   (iterate ()
				 (apply-to-next (lambda (name ...) body ...) (name ...))))))
  (iter-for! ((_ (def ...) body ...) (iter-run (iter-for (def ...) body ...))))
  (iterate-for-impl-impl
   ((_ macro-for-body
	   null recur recur-next ((ivar iter) ...) ((var val) ...) body ...)
	(let-type
	 ((ivar iter) ... (var val) ...)
	 (macro-for-body
	  (set-to-values!
	   var ...
	   (let recur-next ((var var) ...)
		 (if-let-next ((ivar ivar) ...)
		   (let recur ((var var) ...)
			 (define (null) (values iter-null var ...))
			 body ...)
		   (values iter-null var ...))))))))
  (for-macro ((_ body ...) (make-iterator (lambda () body ...))))
  (for!-macro ((_ body ...) (while (not (iter-null? (begin body ...)))))))

(syntax-for-each
 (syntax-rule
  :::
  (_ macro-name let-type)
  (define-syntax*
	(macro-name
	 ((_ ((name value) ...) body ...)
	  (iterate-impl-impl let-type ignore-1 ignore-2 ((name value) ...) body ...))
	 ((_ null ((name value) ...) body ...)
	  (iterate-impl-impl let-type null ignore ((name value) ...) body ...))
	 ((_ null recur ((name value) ...) body ...)
	  (iterate-impl-impl let-type null recur ((name value) ...) body ...)))))
 (iterate let)
 (iterate* let*))

(define (iterator-replace! old new)
  (set-iterator-next-function! old (iterator-next-function new)))
(define (iterator-defer! old new)
  (iterator-replace! old new) (iter-next old))

(define iter-null (iterator (this) iter-null))
(define (iter-null? obj) (eqv? obj iter-null))
;; Setting iter to iter-null on the EOS would make this more predictable but it
;; would also incur an overhead and preclude multi-valued iterators.
(define (iter-next iter) ((iterator-next-function iter) iter))
(define (iter-cons item iter)
  (iterator (this)
	(iterator-replace! this iter)
	item))
(define (iter-peek iter)
  (define item (iter-peek iter))
  (iterator-replace! iter (iter-cons item iter))
  item)

(define-syntax* just
  ((_ item ...) (vector->iter (vector item ...))))
(define (iter-list . args) (list->iter args))
(define (list->iter list)
  (iterate null ((l list))
	(if (null? l)
		(null)
		(values (car l) (cdr l)))))
(define (port->char-iter port)
  (iterator ()
	(define out (read-char port))
	(if (eof-object? out) iter-null out)))
(define (string->iter string)
  (port->char-iter (open-input-string string)))

(define (repeat-string->iter string)
  (define port (open-input-string string))
  (iterator loop ()
	(define next (write-char port))
	(if (eof-object? next)
		(begin (set! port (open-input-string string))
			   (loop))
		next)))
(define (iter->string iter)
  (define out (open-output-string))
  (iter-for-each (lambda (c) (write-char c out)) iter)
  (get-output-string out))
(define (vector->iter vect)
  (iterate null ((i 0))
	(if (< i (vector-length vect))
		(values (vector-ref vect i) (+ i 1))
		(null))))
(define (repeat-vector->iter vect)
  (iterate null loop ((i 0))
	(if (< i (vector-length vect))
		(values (vector-ref vect i) (+ i 1))
		(loop 0))))

(define (repeat-list->iter list)
  (if (null? list)
	  iter-null
	  (iterate null loop ((l list))
		(if (null? l)
			(loop list)
			(values (car l) (cdr l))))))
(define (pairs->iter list)
  (iterate null ((l list))
	(if (pair? l)
		(values (car l) (cdr l))
		(null))))
;; Call branch->iter on tree, then on the items from that iterator recursively.
;; Yield any results which aren't iterators.
;; Skip the initial call if tree-already-iter?.
;; For example:
;; (iter->list (tree->iter '(a (b (c)) d) (lambda (i) (if (pair? i) (list->iter i) i))))
;; => (a b c d)
(define tree->iter
  (case-lambda
	((tree item->iter) (tree->iter tree item->iter #f))
	((tree item->iter tree-already-iter?)
	 (iterate null next
			  ((stack (list (if tree-already-iter? tree (item->iter tree)))))
	   (if (null? stack)
		   (null)
		   (let ((i (iter-next (car stack))))
			 (if (iter-null? i)
				 (next (cdr stack))
				 (let ((branch (item->iter i)))
				   (if (iterator? branch)
					   (next (cons branch stack))
					   (values branch stack))))))))))

(define (iter-recursive-flatten iter)
  (tree->iter iter identity))

(define (iter->list iter)
  (define first (iter-next iter))
  (if (iter-null? first)
	  '()
	  (let* ((output (list first))
			 (output-tail output))
		(iter-for-each
		 (lambda (i)
		   (set-cdr! output-tail (list i))
		   (set! output-tail (cdr output-tail)))
		 iter)
		output)))
(define (iter->vector iter)
  ;; TODO: Implement this by filling/reallocating a large vector and shrinking
  ;; it down afterwards.
  (list->vector (iter->list iter)))

(define (step-iterators its)
  (define gave-null? #f)
  (define result
	(iter-map
	 (lambda (i)
	   (define val (i))
	   (set! gave-null? (iter-null? val))
	   val)
	 (list->iter its)))
  (if gave-null? #f result))

(define (assert-1-arg name args)
  (when (null? args)
	(error name "Expected at least one rest argument." args)))
(define-syntax*
  (define-variadic
	((_ name case-name arg ...)
	 (define name
	   (lambda-cases
		case-name
		((arg ... a) (arg ... a b) (arg ... a b c) (arg ... a b c d) (arg ... . rest))))))
  (lambda-cases
   ((_ macro () cases ...)
	(case-lambda cases ...))
   ((_ macro (macro-case ... last-case) normal-case ...)
	(lambda-cases macro (macro-case ...)
				  (last-case (macro . last-case)) normal-case ...)))
  (map-case
   ((_ f arg ...) (iterator () (apply-to-next f (arg ...))))
   ((_ f . rest)
	(begin
	  (assert-1-arg "iter-map" rest)
	  (iterator ()
		(let ((its (step-iterators rest)))
		  (if its (apply f its) iter-null))))))
  (for-each-case
   ((_ f arg ...)
	(iter-run (iter-map f arg ...)))
   ((_ f . rest)
	(begin
	  (assert-1-arg "iter-for-each" rest)
	  (iter-run (apply iter-map f rest)))))
  (fold-case
   ((_ f seed arg ...)
	(begin
	  (iter-for-each (lambda (arg ...) (set! seed (f seed arg ...))) arg ...)
	  seed))
   ((_ f seed . rest)
	(begin
	  (assert-1-arg "iter-fold" rest)
	  (apply iter-for-each (lambda args (set! seed (apply f seed args))) rest)
	  seed)))
  (scan-case
   ((_ f seed arg ...)
	(iterate null ((seed seed))
	  (apply-to-next*
	   (null)
	   (lambda (seed arg ...)
		 (define out (f seed arg ...))
		 (values out out))
	   seed (arg ...))))
   ((_ f seed . rest)
	(begin
	  (assert-1-arg "iter-scan" rest)
	  (iterate null ((seed seed))
		(let ((its (step-iterators rest)))
		  (if its
			  (let ((out (apply f its)))
				(values out out))
			  (null)))))))
  (count-case
   ((_ pred? arg ...)
	(iter-fold
	 + 0 (iter-map (lambda (arg ...) (if (pred? arg ...) 1 0)) arg ...)))
   ((_ pred? . rest)
	(iter-fold
	 + 0 (apply iter-map (lambda args (if (apply pred? args) 1 0)) rest))))
  (any-case
   ((_ pred? arg ...) (iter-find identity #f (iter-map pred? arg ...)))
   ((_ pred? . rest) (iter-find identity #f (apply iter-map pred? rest))))
  (every-case
   ((_ pred? arg ...) (iter-find not #t (iter-map pred? arg ...)))
   ((_ pred? . rest) (iter-find not #t (apply iter-map pred? rest)))))

(define-variadic iter-map map-case f)
(define-variadic iter-fold fold-case f seed)
(define-variadic iter-for-each for-each-case f)
(define-variadic iter-scan scan-case f seed)
(define-variadic iter-count count-case pred?)
(define-variadic iter-any? any-case pred?)
(define-variadic iter-every? every-case pred?)

(define (iter-run it)
  (while (not (iter-null? (iter-next it)))))
(define iter-last-sentinal (cons 'iter 'last))
(define (iter-last it)
  (define out (iter-fold (lambda (a b) b) iter-last-sentinal it))
  (if (eqv? out iter-last-sentinal)
	  (error #f "Attempt to get last of empty iterator.")
	  out))
(define (iter-filter f iter)
  (iterator next ()
	(let-next ((item iter))
	  (if (f item) item (next)))))
(define (iter-remove f iter)
  (iterator next ()
	(let-next ((item iter))
	  (if (f item) (next) item))))

(define (iter-append . its)
  (iterator (this)
	(if (null? its)
		iter-null
		(let ((next (iter-next (car its))))
		  (if (iter-null? next)
			  (begin
				(set! its (cdr its))
				(when (and (not (null? its)) (null? (cdr its)))
				  (iterator-replace! this (car its)))
				(iter-next this))
			  next)))))

(define iter-iota
  (case-lambda
	((count) (iter-iota count 0 1))
	((count start) (iter-iota count start 1))
	((count start step)
	 (iterate null ((out start) (n 0))
	   (if (< n count)
		   (values out (+ out step) (+ n 1))
		   (null))))))
(define iter-range
  (case-lambda
	((end) (iter-range 0 end 1))
	((start end) (iter-range start end (if (< start end) 1 -1)))
	((start end step)
	 (unless (and (integer? start) (integer? end) (integer? step))
	   (error "iter-range"
			  "Non-integer arguments may cause imprecision errors, try iter-iota."
			  start end step))
	 (define count (- end start))
	 (iter-iota (+ 1 (quotient count step)) start step))))
(define (iter-forever item) (iterator () item))
(define (iter-repeat times item) (iter-take times (iter-forever item)))
(define (iter-duplicate n iter)
  (if (<= n 0)
	  iter-null
	  (iterate ((count 0) (item #f))
		(define item* (if (= count 0) (iter-next iter) item))
		(values item* (remainder (+ count 1) n) item*))))

(define (iter-take n iter)
  (iterate null ((n n))
	(if (= n 0)
		(null)
		(values (iter-next iter) (- n 1)))))
(define (iter-drop n iter)
  (iterator (this)
	(while (< n 0) (iter-next (iter)) (set! n (- n 1)))
	(iterator-defer! this iter)))
(define (iter-take-while pred? iter)
  (iterator (this)
	(let-next ((item iter))
	  (if (pred? item)
		  item
		  (iterator-defer! this iter-null)))))
(define (iter-drop-while pred? iter)
  (iterator recur (this)
	(let-next ((item iter))
	  (if (pred? item)
		  (recur)
		  (begin
			(iterator-replace! this iter)
			item)))))
(define iter-find
  (case-lambda
	((pred? iter) (iter-find pred? #f iter))
	((pred? default iter)
	 (iter-or (iter-next (iter-filter pred? iter))
			  default))))

(define (iter-flatten iter)
  (define sub-iter iter-null)
  (iterator next (this)
	(iter-or
	 (iter-next sub-iter)
	 (let ((outer-next (iter-next iter)))
	   (if (iter-null? outer-next)
		   (iterator-defer! this iter-null)
		   (begin
			 (set! sub-iter outer-next)
			 (next)))))))

(define (iter-reduce f default iter)
  (if-let-next ((a iter))
	(iter-fold f a iter)
	default))
