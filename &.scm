(define-module (shorthand &)
  #:use-module (guile)
  #:use-module (ice-9 match)
  #:use-module (shorthand strings)
  #:use-module (shorthand lists)
  #:use-module (shorthand io)
  #:use-module (shorthand syntax)
  #:use-module (shorthand reader)
  #:use-module (shorthand utils)
  #:export (&))

(define read-upto-&
  (read-many (read-matching (lambda (x) (non char=? x #\&)))))

(define &-read
  (map-reader cadr (read-sequence (read-matching #\&) read)))

(define &-item
  (map-reader
   (match-lambda
     [() eof]
     [(x) (list (list->string x))]
     [(x y) (list (list->string x) y)])
   (read-trying read-upto-& &-read)))

(define &-items
  (map-reader
   (lambda (res) (apply append! res))
   (read-some &-item)))

(define (syntax->string stx)
  "Turns a syntax object into a syntax object which evaluates to a string"
  (define dat (syntax->datum stx))
  (if (string? dat)
      stx
      #`(->string #,stx)))

(define-syntax &
  (lambda (stx)
    (syntax-case stx ()
      [(& str) 
       (let [(strings (map (lambda (x)
                             (syntax->string
                              (datum->syntax #'str x #:source #'str)))
                           (with-input-from-string (syntax->datum #'str)
                             &-items)))]
                           
         (cons #'string-append/shared strings))])))
