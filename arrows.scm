(define-module (shorthand arrows)
  #:use-module (guile))

(define-syntax ->
  (syntax-rules ()
    [(-> value)                     value]
    [(-> value (form ...) rest ...) (-> (form ... value) rest ...)]
    [(-> value form rest ...)       (-> value (form) rest ...)]))

(export ->)
  
