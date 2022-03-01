(define-module (shorthand modules)
  #:use-module (guile))

(define-syntax-rule (define-blank-module module-name)
  (resolve-module 'module-name #:ensure #t))

(define-syntax define-in
  (syntax-rules ()
    [(define-in mod name val)
     (module-define! (resolve-module 'mod) 'name val)]
    [(define-in mod (name args ...) vals ...)
     (module-define! (resolve-module 'mod) 'name (lambda (args ...) vals ...))]))
