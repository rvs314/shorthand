(define-module (shorthand strings)
  #:use-module (guile))

(define-public (without-suffix string suffix)
  "Removes a suffix from a string"
  (if (string-suffix? suffix string)
      (string-drop-right string (string-length suffix))
      string))

(define-public (->string obj)
  "Converts an object into a string"
  (format #f "~a" obj))
