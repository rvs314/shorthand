(define-module (shorthand syntax)
  #:use-module (guile)
  #:use-module (system syntax)
  #:use-module (srfi srfi-1))

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

#|
expand-once is based on that written by Mark H. Weaver on the guile-user mailing list:
https://lists.gnu.org/archive/html/guile-user/2018-05/msg00042.html.
|#

(define (syntax-local-type id)
  (let-values (((type value) (syntax-local-binding id)))
    type))

(define (syntax-local-value id)
  (let-values (((type value) (syntax-local-binding id)))
    value))

(define-syntax expand-once
  (lambda (stx)
    (syntax-case stx ()
      [(_ (rator rand ...))
       (and (identifier? #'rator)
            (eq? 'macro (syntax-local-type #'rator)))
       #`(quote #,((syntax-local-value #'rator) #'(rator rand ...)))]
      [(_ x) #''x])))

(export expand-once)
