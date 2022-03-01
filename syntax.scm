(define-module (shorthand syntax)
  #:use-module (rnrs))

(define-public (map-syntax function syntax-object)
  "Converts the syntax object to a datum, applies the function to the datum,
   then converts it back into a syntax object"
  (datum->syntax syntax-object
                 (function (syntax->datum syntax-object))
                 #:source syntax-object))
  
(define-public (syntax->list stx)
  "Turns a syntax object which contains an application into a list of syntax objects"
  (if (list? stx) stx
      (syntax-case stx ()
        [() '()]
        [(a b ...) (cons #'a (syntax->list #'(b ...)))])))

;; Defines a bunch of objects, optionally public
(define-syntax define-all
  (syntax-rules (public)
    [(define-all) (values)]
    [(define-all public) (values)]
    [(define-all public a b rest ...)
     (begin (define-public a b)
            (define-all public rest ...))]
    [(define-all a b rest ...)
     (begin (define a b)
            (define-all rest ...))]))

(export define-all)
