(define-module (shorthand io)
  #:use-module (guile)
  #:use-module (oop goops))

(define-public (puts . objs)
  (cond [(null? objs)       (newline)]
        [(null? (cdr objs)) (display (car objs))
                            (newline)]
        [else               (display (car objs))
                            (display " ")
                            (apply puts (cdr objs))]))

(define* (eof? #:optional (port (current-input-port)))
  "Returns true if the current-input-port or 'port' has hit an end-of-file"
  (eof-object? (peek-char port)))

(export eof?)
