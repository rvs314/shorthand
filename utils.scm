(define-module (shorthand utils)
  #:use-module (rnrs))

(define* (maybe fn val #:optional else)
  "Applys the function if the value isn't false, otherwise returns the alternative.
   Equivalent to Haskell's 'maybe :: b → (a → b) → Maybe a → b' function"
  (if val (fn val) else)) 

(define (apply-if pred fn val)
  "Applies a function to a value if the predicate holds for the given value"
  (if (pred val) (fn val) val))

(export maybe apply-if)
