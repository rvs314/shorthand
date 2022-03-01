(define-module (shorthand $)
  #:use-module (guile))

(define (fmt-string str)
  ())

(define-syntax (fmt stx)
  (syntax-case stx ()
    [(fmt str)]))

;; ($ "2 + 2 is $(+ 2 2)") ⇒ (string-append/shared "2 + 2 is " (+ 2 2))
