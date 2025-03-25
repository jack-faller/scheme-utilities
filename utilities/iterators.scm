(define-library (utilities iterators)
  (export
   ;; Primitive creation.
   make-iterator make-iterator*
   iterator? iterator-defer! iterator-replace!
   null null? empty or
   iterator iterator*
   let-next if-let-next apply-to-next apply-to-next* for for!

   ;; Access.
   find next peek

   ;; Creation.
   just
   from-list repeat-list from-pairs
   from-string repeat-string
   from-vector repeat-vector
   port-chars port-bytes port-lines port-expressions
   forever repeat
   from-tree
   range iota
   iterate until while

   ;; Manipulation.
   map scan filter remove
   take take-while drop drop-while
   flatten recursive-flatten
   duplicate
   cons append
   zip

   ;; TODO: nth!
   ;; TODO: (collect! sink iterator ...) <- allows multi-valued sinks
   ;; I actually can't think of a use-case for collect!, given all the sink functions are also implemented here.
   ;; TODO: rename all functions to remove iter

   ;; Collection.
   any? every? count
   to-string to-list to-vector
   for-each run last
   fold reduce)

  (import (rename (scheme base)
				  (null? base/null?)
				  (or base/or)
				  (cons base/cons)
				  (for-each base/for-each)
				  (map base/map)
				  (append base/append))
		  (rename (utilities)
				  (until utils/until)
				  (while utils/while))
		  (scheme case-lambda) (utilities syntax)))

(define-record-type <iterator>
  (make-iterator next-function)
  iterator?
  (next-function iterator-next-function set-iterator-next-function!))

(define-record-type <null> (make-null-singleton) null-singleton?)
(define null (make-null-singleton))
(define (null? obj) (eqv? obj null))

(define-syntax make-iterator*
  (syntax-rules ()
	((_ () body ...)
	 (make-iterator* ignored-1 (ignored-2) body ...))
	((_ (this) body ...)
	 (make-iterator* ignored (this) body ...))
	((_ name () body ...)
	 (make-iterator* name (ignored-2) body ...))
	((_ recur (this) body ...)
	 (make-iterator (lambda (this) (let recur () body ...))))))

(define-syntax*
  (macro-map*
   ((_ (name ...) ((value ...) ...) body ...)
	(let-syntax ((body-macro (syntax-rules ignore ()
							   ((_ name ...) (begin body ...)))))
	  (body-macro value ...) ...)))
  (macro-map
   ((_ name (value ...) body ...) (macro-map* (name) ((value) ...) body ...)))
  (or
   ((_ item) item)
   ((_ first item ...)
	(let ((i first)) (if (null? i) (or item ...) i))))
  (apply-to-next*-impl
   ((_ false-case f arg ... ()) (f arg ...))
   ((_ false-case f arg ... (first iterator ...))
	(let ((it (next first)))
	  (if (null? it)
		  (false-case)
		  (apply-to-next*-impl false-case f arg ... it (iterator ...))))))
  (apply-to-next*
   ((_ false-case f arg ... (iterator ...))
	(let ((else (lambda () false-case)))
	  (apply-to-next*-impl else f arg ... (iterator ...)))))
  (apply-to-next
   ((_ f arg ... (iterator ...))
	(apply-to-next* null f arg ... (iterator ...))))
  (let-next
	  ((_ ((name val) ...) body ...)
	   (apply-to-next (lambda (name ...) body ...) (val ...))))
  (if-let-next
	  ((_ ((name val) ...) then else)
	   (apply-to-next* else (lambda (name ...) then) (val ...))))
  (iterate-impl
   ((_ let null recur ((name value) ...) body ...)
	(let ((name value) ...)
	  (make-iterator* ()
		(set-to-values!
		 name ...
		 (let ((null (lambda () (values null name ...))))
		   (let recur ((name name) ...)
			 body ...)))))))
  (zip-impl
   ((_ () (names ...))
	(make-iterator* () (apply-to-next values (names ...))))
   ((_ (it its ...) (names ...))
	(let ((name it)) (zip-impl (its ...) (names ... name)))))
  (zip
   ((_ its ...)
	(zip-impl (its ...) ())))
  (for ((_ ((name iter) ...) body ...)
			 (let ((name iter) ...)
			   (make-iterator* ()
				 (apply-to-next (lambda (name ...) body ...) (name ...))))))
  (for! ((_ (def ...) body ...) (run (for (def ...) body ...)))))

(syntax-map
 (syntax-rule
  :::
  (_ macro-name let-type)
  (define-syntax*
	(macro-name
	 ((_ ((name value) ...) body ...)
	  (iterate-impl let-type ignore-1 ignore-2 ((name value) ...) body ...))
	 ((_ null ((name value) ...) body ...)
	  (iterate-impl let-type null ignore ((name value) ...) body ...))
	 ((_ null recur ((name value) ...) body ...)
	  (iterate-impl let-type null recur ((name value) ...) body ...)))))
 (iterator let)
 (iterator* let*))

(define (iterator-replace! old new)
  (set-iterator-next-function! old (iterator-next-function new)))
(define (iterator-defer! old new)
  (iterator-replace! old new) (next old))

(define empty (make-iterator* (this) null))
;; Setting iter to null on the EOS would make this more predictable but it
;; would also incur an overhead and preclude multi-valued iterators.
(define (next iter) ((iterator-next-function iter) iter))
(define (cons item iter)
  (make-iterator* (this)
	(iterator-replace! this iter)
	item))
(define (peek iter)
  (define item (peek iter))
  (iterator-replace! iter (cons item iter))
  item)

(define-syntax* just
  ((_ item ...) (vector (from-vector item ...))))
(define (from-list list)
  (iterator null ((l list))
	(if (base/null? l)
		(null)
		(values (car l) (cdr l)))))

(define until
  (syntax-variadic-lambda
   (stop? f) (a b c d)
   (syntax-rules ()
	 ((_ s? f arg ...)
	  (iterator ()
		(let ((val (f arg ...)))
		  (if (s? val) null val))))
	 ((_ s? f arg ... . rest)
	  (iterator ()
        (let ((val (apply f arg ... rest)))
		  (if (s? val) null val)))))))
(define while
  (syntax-variadic-lambda
   (continue? f) (a b c d)
   (syntax-rules ()
	 ((_ s? f arg ...)
	  (iterator ()
		(let ((val (f arg ...)))
		  (if (s? val) val null))))
	 ((_ s? f arg ... . rest)
	  (iterator ()
        (let ((val (apply f arg ... rest)))
		  (if (s? val) val null)))))))

(define iterate
  (case-lambda
	((f seed)
	 (iterator ((seed seed)) (let ((next (f seed))) (values next next))))
	((f seed stop?)
	 (iterator null ((seed seed))
			   (let ((next (f seed)))
				 (if (stop? next) (null) (values next next)))))))

(define (port-chars port) (until eof-object? read-char port))
(define (port-bytes port) (until eof-object? read-u8 port))
(define (port-lines port) (until eof-object? read-line port))
(define (port-expressions port) (until eof-object? read port))

(define (from-string string)
  (from-port-chars (open-input-string string)))

(define (repeat-string string)
  (define port (open-input-string string))
  (make-iterator* loop ()
	(define next (write-char port))
	(if (eof-object? next)
		(begin (set! port (open-input-string string))
			   (loop))
		next)))
(define (to-string iter)
  (define out (open-output-string))
  (for-each (lambda (c) (write-char c out)) iter)
  (get-output-string out))
(define (from-vector vect)
  (iterator null ((i 0))
	(if (< i (vector-length vect))
		(values (vector-ref vect i) (+ i 1))
		(null))))
(define (repeat-vector vect)
  (iterator null loop ((i 0))
	(if (< i (vector-length vect))
		(values (vector-ref vect i) (+ i 1))
		(loop 0))))

(define (repeat-list list)
  (if (base/null? list)
	  null
	  (iterator null loop ((l list))
		(if (base/null? l)
			(loop list)
			(values (car l) (cdr l))))))
(define (from-pairs list)
  (iterator null ((l list))
	(if (pair? l)
		(values (car l) (cdr l))
		(null))))
;; from-Call branch on tree, then on the items from that iterator recursively.
;; Yield any results which aren't iterators.
;; Skip the initial call if tree-already-iter?.
;; For example:
;; (from-to-list (tree '(from-a (b (c)) d) (lambda (i) (if (pair? i) (list i) i))))
;; => (a b c d)
(from-define tree
  (case-lambda
	((from-tree item) (from-tree from-tree item #f))
	((from-tree item tree-already-iter?)
	 (iterator null next
			  ((from-stack (list (if tree-already-iter? tree (item tree)))))
	   (if (base/null? stack)
		   (null)
		   (let ((i (next (car stack))))
			 (if (null? i)
				 (next (cdr stack))
				 (from-let ((branch (item i)))
				   (if (iterator? branch)
					   (next (base/cons branch stack))
					   (values branch stack))))))))))

(define (recursive-flatten iter)
  (from-tree iter identity))

(define (to-list iter)
  (define first (next iter))
  (if (null? first)
	  '()
	  (let* ((output (list first))
			 (output-tail output))
		(for-each
		 (lambda (i)
		   (set-cdr! output-tail (list i))
		   (set! output-tail (cdr output-tail)))
		 iter)
		output)))
(define (to-vector iter)
  ;; TODO: Implement this by filling/reallocating a large vector and shrinking
  ;; it down afterwards.
  (list->vector (to-list iter)))

(define (step-iterators its)
  (define gave-null? #f)
  (define result
	(map
	 (lambda (i)
	   (define val (i))
	   (set! gave-null? (null? val))
	   val)
	 (from-list its)))
  (if gave-null? #f result))

(define (assert-1-arg name args)
  (when (base/null? args)
	(error name "Expected at least one rest argument." args)))

(define map
  (syntax-variadic-lambda
   (f) (a b c d)
   (syntax-rule (_ f arg ...) (make-iterator* () (apply-to-next f (arg ...))))
   ((f . rest)
	(begin
	  (assert-1-arg "map" rest)
	  (make-iterator* ()
				(let ((its (step-iterators rest)))
				  (if its (apply f its) null)))))))
(define for-each
  (syntax-variadic-lambda
   (f) (a b c d)
   (syntax-rule (_ f arg ...) (run (map f arg ...)))
   ((f . rest)
	(begin
	  (assert-1-arg "for-each" rest)
	  (run (apply map f rest))))))
(define fold
  (syntax-variadic-lambda
   (f) (a b c d)
   (syntax-rule
	(_ f seed arg ...)
	(begin
	  (for-each (lambda (arg ...) (set! seed (f seed arg ...))) arg ...)
	  seed))
   ((f seed . rest)
	(begin
	  (assert-1-arg "fold" rest)
	  (apply for-each (lambda args (set! seed (apply f seed args))) rest)
	  seed))))
(define scan
  (syntax-variadic-lambda
   (f) (a b c d)
   (syntax-rule
	(_ f seed arg ...)
	(iterator null ((seed seed))
			 (apply-to-next*
			  (null)
			  (lambda (seed arg ...)
				(define out (f seed arg ...))
				(values out out))
			  seed (arg ...))))
   ((f seed . rest)
	(begin
	  (assert-1-arg "scan" rest)
	  (iterator null ((seed seed))
			   (let ((its (step-iterators rest)))
				 (if its
					 (let ((out (apply f its)))
					   (values out out))
					 (null))))))))
(define count
  (syntax-variadic-lambda
   (pred?) (a b c d)
   (syntax-rule
	(_ pred? arg ...)
	(fold
	 + 0 (map (lambda (arg ...) (if (pred? arg ...) 1 0)) arg ...)))
   ((pred? . rest)
	(fold
	 + 0 (apply map (lambda args (if (apply pred? args) 1 0)) rest)))))
(define any?
  (syntax-variadic-lambda
   (pred?) (a b c d)
   (syntax-rule
	(_ pred? arg ...) (find identity #f (map pred? arg ...)))
   ((pred? . rest) (find identity #f (apply map pred? rest)))))
(define every?
  (syntax-variadic-lambda
   (pred?) (a b c d)
   (syntax-rule (_ pred? arg ...) (find not #t (map pred? arg ...)))
   ((pred? . rest) (find not #t (apply map pred? rest)))))

(define (run it)
  (utils/while (not (null? (next it)))))
(define last-sentinal (base/cons 'iter 'last))
(define (last it)
  (define out (fold (lambda (a b) b) last-sentinal it))
  (if (eqv? out last-sentinal)
	  (error #f "Attempt to get last of empty iterator.")
	  out))
(define (filter f iter)
  (make-iterator* next ()
	(let-next ((item iter))
	  (if (f item) item (next)))))
(define (remove f iter)
  (make-iterator* next ()
	(let-next ((item iter))
	  (if (f item) (next) item))))

(define (append . its)
  (make-iterator* (this)
	(if (base/null? its)
		null
		(let ((next (next (car its))))
		  (if (null? next)
			  (begin
				(set! its (cdr its))
				(when (and (not (base/null? its)) (base/null? (cdr its)))
				  (iterator-replace! this (car its)))
				(next this))
			  next)))))

(define iota
  (case-lambda
	((count) (iota count 0 1))
	((count start) (iota count start 1))
	((count start step)
	 (iterator null ((out start) (n 0))
	   (if (< n count)
		   (values out (+ out step) (+ n 1))
		   (null))))))
(define range
  (case-lambda
	((end) (range 0 end 1))
	((start end) (range start end (if (< start end) 1 -1)))
	((start end step)
	 (unless (and (integer? start) (integer? end) (integer? step))
	   (error "range"
			  "Non-integer arguments may cause imprecision errors, try iota."
			  start end step))
	 (define count (- end start))
	 (iota (+ 1 (quotient count step)) start step))))
(define (forever item) (make-iterator* () item))
(define (repeat times item) (take times (forever item)))
(define (duplicate n iter)
  (if (<= n 0)
	  null
	  (iterator ((count 0) (item #f))
		(define item* (if (= count 0) (next iter) item))
		(values item* (remainder (+ count 1) n) item*))))

(define (take n iter)
  (iterator null ((n n))
	(if (= n 0)
		(null)
		(values (next iter) (- n 1)))))
(define (drop n iter)
  (make-iterator* (this)
	(utils/while (< n 0) (next (iter)) (set! n (- n 1)))
	(iterator-defer! this iter)))
(define (take-while pred? iter)
  (make-iterator* (this)
	(let-next ((item iter))
	  (if (pred? item)
		  item
		  (iterator-defer! this empty)))))
(define (drop-while pred? iter)
  (make-iterator* recur (this)
	(let-next ((item iter))
	  (if (pred? item)
		  (recur)
		  (begin
			(iterator-replace! this iter)
			item)))))
(define find
  (case-lambda
	((pred? iter) (find pred? #f iter))
	((pred? default iter)
	 (or (next (filter pred? iter))
			  default))))

(define (flatten iter)
  (define sub-iter null)
  (make-iterator* next (this)
	(or
	 (next sub-iter)
	 (let ((outer-next (next iter)))
	   (if (null? outer-next)
		   (iterator-defer! this empty)
		   (begin
			 (set! sub-iter outer-next)
			 (next)))))))

(define (reduce f default iter)
  (if-let-next ((a iter))
	(fold f a iter)
	default))
