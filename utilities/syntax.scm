(define-library (utilities syntax)
  (export define-syntax* syntax-rule syntax-apply syntax-map syntax-variadic-lambda syntax-first syntax-second syntax-third)
  (import (scheme base) (scheme case-lambda)))

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
;; This may not be portable as I'm not sure if let-syntax needs to do recursive expansion.
(define-syntax*
  (syntax-apply ((_ rule argument ...) (begin (define-syntax s rule) (s argument ...))))
  (syntax-map ((_ rule arguments ...) (begin (define-syntax s rule) (s . arguments) ...))))

(define-syntax*
  (syntax-first ((_ a . rest) a))
  (syntax-second ((_ a b . rest) b))
  (syntax-third ((_ a b c . rest) c)))

(define-syntax* syntax-variadic-lambda
  ((_ (parameter ...) (rest-parameter ...) variable-case)
   (let-syntax ((body variable-case))
	 (syntax-variadic-lambda
	  (parameter ...) (rest-parameter ...) variable-case
	  ((parameter ... rest-parameter ... . rest)
	   (body parameter ... rest-parameter ... . rest)))))
  ((_ (parameter ...) (rest-parameter ...) variable-case final-case)
   (letrec-syntax
	   ((body variable-case)
		(collect-cases
		 (syntax-rules ::: ()
		   ((_ used-items () (cases :::))
			(case-lambda cases ::: final-case))
		   ((_ (used-item :::) (this-item item :::) (cases :::))
			(collect-cases
			 (used-item ::: this-item) (item :::)
			 (cases
			  :::
			  ((parameter ... used-item ::: this-item)
			   (body parameter ... used-item ::: this-item))))))))
	 (collect-cases () (rest-parameter ...) ()))))
