(define-library (utilities syntax)
  (export define-syntax* syntax-rule syntax-apply syntax-map syntax-variadic-lambda)
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

(define-syntax* syntax-variadic-lambda
  ((_ (parameter ...) (rest-parameter ...))
   (letrec-syntax
	   ((body syntax)
		(collect-cases
		 (syntax-rules ::: ()
		   ((_ (used-item :::) () (cases :::))
			(case-lambda
			  cases :::
			  ((parameter ... used-item ::: . rest)
			   (body parameter ... used-item ::: . rest))))
		   ((_ (used-item :::) (this-item item :::) (cases :::))
			(collect-cases
			 (used-item ::: this-item) (item :::)
			 (cases
			  :::
			  ((parameter ... used-item ::: this-item)
			   (body parameter ... used-item ::: this-item))))))))
	 (collect-cases () (rest-parameter ...) ()))))

