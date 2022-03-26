(define-module (shothand sugar)
  #:use-module (guile))

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

(define-syntax ->
  (syntax-rules ()
    [(-> value)                     value]
    [(-> value (form ...) rest ...) (-> (form ... value) rest ...)]
    [(-> value form rest ...)       (-> value (form) rest ...)]))

(export define-all ->)
