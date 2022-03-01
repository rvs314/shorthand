(define-module (shorthand functions)
  #:use-module (guile))

(define-public (non fn . args)
  "Inverts a function application"
  (if (null? args)
      (lambda xs (apply non fn xs))
      (not (apply fn args))))
