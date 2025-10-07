(define-library (utilities iterators)
  (export
   ;; Primitive creation.
   make-iterator make-iterator*
   iterator? iterator-defer! iterator-replace!
   (rename (iter:null null))
   (rename (iter:null? null?))
   (rename (iter:or or))
   empty
   iterator iterator*
   let-next! if-let-next! apply-to-next! apply-to-next!* for for!

   ;; Access.
   find! next! peek!

   ;; Creation.
   just
   from-list repeat-list from-pairs
   from-string repeat-string
   from-vector repeat-vector
   port-chars port-bytes port-lines
   forever repeat
   from-tree
   range iota
   iterate until while

   ;; Manipulation.
   (rename (iter:map map))
   scan filter remove
   take take-while drop drop-while
   flatten recursive-flatten
   duplicate
   (rename (iter:cons cons))
   (rename (iter:append append))
   zip
   merge interleave select

   ;; TODO: replace these all with sinks.
   ;; Collection.
   submit-to! collect! run! for-each!)
  (import (scheme base)
          (rename (utilities)
                  (until utils:until)
                  (while utils:while))
          (scheme case-lambda)
	  (utilities syntax)
	  (prefix (utilities sinks) sink:)))

(define-record-type <iterator>
  (make-iterator next-function)
  iterator?
  (next-function iterator-next-function set-iterator-next-function!))

(define-singleton <null> iter:null iter:null?)

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
  (iter:or
   ((_ item) item)
   ((_ first item ...)
    (let ((i first)) (if (iter:null? i) (iter:or item ...) i))))
  (apply-to-next!*-impl
   ((_ false-case f arg ... ()) (f arg ...))
   ((_ false-case f arg ... (first iterator ...))
    (let ((it (next! first)))
      (if (iter:null? it)
          (false-case)
          (apply-to-next!*-impl false-case f arg ... it (iterator ...))))))
  (apply-to-next!*
   ((_ false-case f arg ... (iterator ...))
    (let ((else (lambda () false-case)))
      (apply-to-next!*-impl else f arg ... (iterator ...)))))
  (apply-to-next!
   ((_ f arg ... (iterator ...))
    (apply-to-next!* iter:null f arg ... (iterator ...))))
  (let-next!
      ((_ ((name val) ...) body ...)
       (apply-to-next! (lambda (name ...) body ...) (val ...))))
  (if-let-next!
      ((_ ((name val) ...) then else)
       (apply-to-next!* else (lambda (name ...) then) (val ...))))
  (iterator-impl
   ((_ let null-name recur () body ...)
    (make-iterator* recur (this)
      (define (null-name) (iterator-defer! this empty))
      body ...))
   ((_ let null-name recur ((name value) ...) body ...)
    (let ((name value) ...)
      (make-iterator* (this)
        (set-to-values!*
         name ...
         (let ((null-name (lambda ()
                            (iterator-replace! this empty)
                            (values iter:null name ...))))
           (let recur ((name name) ...)
             body ...)))))))
  (zip-impl
   ((_ () (names ...))
    (make-iterator* () (apply-to-next! values (names ...))))
   ((_ (it its ...) (names ...))
    (let ((name it)) (zip-impl (its ...) (names ... name)))))
  (zip
   ((_ its ...)
    (zip-impl (its ...) ())))
  (for ((_ ((name iter) ...) body ...)
             (let ((name iter) ...)
               (iterator null ()
                 (apply-to-next!* (null) (lambda (name ...) body ...) (name ...))))))
  (for! ((_ (def ...) body ...) (run (for (def ...) body ...)))))

(syntax-map
 (syntax-rule
  :::
  (_ macro-name let-type)
  (define-syntax*
    (macro-name
     ((_ ((name value) ...) body ...)
      (iterator-impl let-type ignore-1 ignore-2 ((name value) ...) body ...))
     ((_ null ((name value) ...) body ...)
      (iterator-impl let-type null ignore ((name value) ...) body ...))
     ((_ null recur ((name value) ...) body ...)
      (iterator-impl let-type null recur ((name value) ...) body ...)))))
 (iterator let)
 (iterator* let*))

(define (empty-func this) iter:null)
(define empty (make-iterator empty-func))

;; Each function should be associated with only one iterator.
(define (iterator-replace! old new)
  (set-iterator-next-function! old (iterator-next-function new))
  (set-iterator-next-function! new empty-func))
(define (iterator-defer! old new)
  (iterator-replace! old new) (next! old))

;; Setting iter to null on the EOS would make this more predictable but it
;; would also incur an overhead and preclude multi-valued iterators.
(define (next! iter) ((iterator-next-function iter) iter))

(define (iter:cons item iter)
  (make-iterator* (this)
    (iterator-replace! this iter)
    item))
(define (peek! iter)
  (define item (peek! iter))
  (iterator-replace! iter (iter:cons item iter))
  item)

(define-syntax* just
  ((_ item ...) (from-vector (vector item ...))))
(define (from-list list)
  (iterator null ((l list))
    (if (null? l)
        (null)
        (values (car l) (cdr l)))))

(define until
  (syntax-variadic-lambda
   (stop? f) (a b c d)
   (syntax-rules ()
     ((_ s? f arg ...)
      (iterator ()
        (let ((val (f arg ...)))
          (if (s? val) iter:null val))))
     ((_ s? f arg ... . rest)
      (iterator ()
        (let ((val (apply f arg ... rest)))
          (if (s? val) iter:null val)))))))
(define while
  (syntax-variadic-lambda
   (continue? f) (a b c d)
   (syntax-rules ()
     ((_ s? f arg ...)
      (iterator ()
        (let ((val (f arg ...)))
          (if (s? val) val iter:null))))
     ((_ s? f arg ... . rest)
      (iterator ()
        (let ((val (apply f arg ... rest)))
          (if (s? val) val iter:null)))))))

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

(define (from-string string)
  (port-chars (open-input-string string)))

(define (repeat-string string)
  (define port (open-input-string string))
  (make-iterator* loop ()
    (define next (write-char port))
    (if (eof-object? next)
        (begin (set! port (open-input-string string))
               (loop))
        next)))
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
  (if (null? list)
      empty
      (iterator null loop ((l list))
        (if (null? l)
            (loop list)
            (values (car l) (cdr l))))))
(define (from-pairs list)
  (iterator null ((l list))
    (if (pair? l)
        (values (car l) (cdr l))
        (null))))
;; Call branch on tree, then on the items from that iterator recursively.
;; Yield any results which aren't iterators.
;; Skip the initial call if tree-already-iter?.
;; For example:
;; (to-list (tree '(a (b (c)) d) (lambda (i) (if (pair? i) (from-list i) i))))
;; => (a b c d)
(define from-tree
  (case-lambda
    ((tree tree->items) (from-tree tree tree->items #f))
    ((tree tree->items tree-already-iter?)
     (iterator null next
       ((stack (list (if tree-already-iter? tree (tree->items tree)))))
       (if (null? stack)
           (null)
           (let ((i (next (car stack))))
             (if (iter:null? i)
                 (next (cdr stack))
                 (let ((branch (tree->items i)))
                   (if (iterator? branch)
                       (next (cons branch stack))
                       (values branch stack))))))))))

(define (recursive-flatten iter)
  (from-tree iter identity))

(define (step-iterators! its)
  (define gave-null? #f)
  (define result
    (iter:map
     (lambda (i)
       (define val (i))
       (set! gave-null? (iter:null? val))
       val)
     (from-list its)))
  (if gave-null? #f result))

(define (assert-1-arg name args)
  (when (null? args)
    (error name "Expected at least one rest argument." args)))

(define iter:map
  (syntax-variadic-lambda
   (f) (a b c d)
   (syntax-rule (_ f arg ...) (iterator () (apply-to-next! f (arg ...))))
   ((f . rest)
    (begin
      (assert-1-arg "map" rest)
      (iterator ()
        (let ((its (step-iterators! rest)))
          (if its (apply f its) iter:null)))))))
(define scan
  (syntax-variadic-lambda
   (f) (a b c d)
   (syntax-rule
    (_ f seed arg ...)
    (iterator null ((seed seed))
      (apply-to-next!*
       (null)
       (lambda (seed arg ...)
         (define out (f seed arg ...))
         (values out out))
       seed (arg ...))))
   ((f seed . rest)
    (begin
      (assert-1-arg "scan" rest)
      (iterator null ((seed seed))
        (let ((its (step-iterators! rest)))
          (if its
              (let ((out (apply f its)))
                (values out out))
              (null))))))))

(define (filter pred? iter)
  (make-iterator* next ()
    (let-next! ((item iter))
      (if (pred? item) item (next)))))
(define (remove pred? iter)
  (make-iterator* next ()
    (let-next! ((item iter))
      (if (pred? item) (next) item))))

(define (iter:append . its)
  (make-iterator* (this)
    (if (null? its)
        iter:null
        (let ((next (next! (car its))))
          (if (iter:null? next)
              (begin
                (set! its (cdr its))
                (when (and (not (null? its)) (null? (cdr its)))
                  (iterator-replace! this (car its)))
                (next! this))
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
(define (forever item) (iterator () item))
(define (repeat times item) (take times (forever item)))
(define (duplicate n iter)
  (if (<= n 0)
      iter:null
      (iterator ((count 0) (item #f))
        (define item* (if (= count 0) (next! iter) item))
        (values item* (remainder (+ count 1) n) item*))))

(define (take n iter)
  (iterator null ((n n))
    (if (= n 0)
        (null)
        (values (next! iter) (- n 1)))))
(define (drop n iter)
  (make-iterator* (this)
    (utils:while (< n 0) (next! (iter)) (set! n (- n 1)))
    (iterator-defer! this iter)))
(define (take-while pred? iter)
  (make-iterator* (this)
    (let-next! ((item iter))
      (if (pred? item)
          item
          (iterator-defer! this empty)))))
(define (drop-while pred? iter)
  (make-iterator* recur (this)
    (let-next! ((item iter))
      (if (pred? item)
          (recur)
          (begin
            (iterator-replace! this iter)
            item)))))

(define (flatten iter)
  (define sub-iter iter:null)
  (make-iterator* next (this)
    (iter:or
     (next! sub-iter)
     (let ((outer-next (next! iter)))
       (if (iter:null? outer-next)
           (iterator-defer! this empty)
           (begin
             (set! sub-iter outer-next)
             (next)))))))

(define (merge pred? when-true when-false)
  (define stored iter:null)
  (define stored-from #f)
  (make-iterator* (this)
    (when (iter:null? stored)
      (set! stored-from (not stored-from))
      (set! stored (next! (if stored-from when-true when-false))))
    (if (iter:null? stored)
	(iterator-defer! this (if stored-from when-false when-true))
	(let ((temp (next! (if stored-from when-false when-true))))
	  (if (iter:null? temp)
	      (begin
		(iterator-replace! this (if stored-from when-true when-false))
		stored)
	      (let-values (((true false) (if stored-from
					     (values stored temp)
					     (values temp stored))))
		(let ((result (pred? true false)))
		  (set! stored-from (not result))
		  (set! stored (if result false true))
		  (if result true false))))))))

(define interleave
  (case-lambda
    ((int-iter vector-of-iterators)
     (iterator null ()
       (if-let-next!
        ((which int-iter))
        (let ((target (vector-ref which vector-of-iterators)))
          (if-let-next! ((val target)) val (null)))
        (null))))
    ((bool-iter false-iter true-iter)
     (iterator null ()
       (if-let-next!
        ((which? bool-iter))
        (let ((target (if which? true-iter false-iter)))
          (if-let-next! ((val target)) val (null)))
        (null))))))
(define select
  (case-lambda
    ((int-iter vector-of-iterators)
     (define len (vector-length vector-of-iterators))
     (when (= len 0)
       (error "select" "Expected at least one iterator." vector-of-iterators))
     (iterator null ()
       (if-let-next!
           ((which int-iter))
         (begin
           ;; Error when return value is too large.
           (when (>= which len)
             (vector-ref vector-of-iterators which))
           (let loop ((i 0) (result #f))
             (if (>= i len)
                 result
                 (if-let-next!
                     ((next (vector-ref vector-of-iterators i)))
                   (loop (++ i) (if (= i which) next #f))
                   (null)))))
         (null))))
    ((bool-iter false-iter true-iter)
     (for ((which? bool-iter) (false false-iter) (true true-iter))
       (if which? true false)))))


(define for-each!
  (syntax-variadic-lambda
   (f) (a b c d)
   (syntax-rule (_ f arg ...) (run! (iter:map f arg ...)))
   ((f . rest)
    (begin
      (assert-1-arg "for-each" rest)
      (run! (apply iter:map f rest))))))
(define (run! it)
  (utils:while (not (iter:null? (next! it)))))
(define (submit-to! sink it)
  (for-each! (lambda (i) (sink:submit! sink i)) it))
(define (collect! sink it)
  (submit-to! sink it)
  (sink:finish! sink))
