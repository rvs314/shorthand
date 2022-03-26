(define-module (shorthand continuations)
  #:use-module (guile))

(define-syntax let/cc
  (syntax-rules ()
    [(let/cc name body body* ...)
     (call/cc (lambda (name) body body* ...))]))

(export let/cc)
