(define-module (shorthand modules)
  #:use-module (guile)
  #:export (define-blank-module define-in define-syntax-in))

(define-syntax-rule (define-blank-module module-name)
  (resolve-module 'module-name #:ensure #t))

(define-syntax define-in
  (syntax-rules ()
    [(define-in mod (name args ...) vals ...)
     (define-in mod name (lambda (args ...) vals ...))]
    [(define-in mod name val)
     (module-define! (resolve-module 'mod) 'name val)]))

(define-syntax define-syntax-in
  (syntax-rules ()
    [(define-syntax-in module name binding)
     (define-in module name
       (make-syntax-transformer name 'macro binding))]))
  
;; (define-syntax-in (cart base) name
;;   (*syntax-transformer*))
