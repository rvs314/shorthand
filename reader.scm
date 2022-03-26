(define-module (shorthand reader)
  #:use-module (guile)
  #:use-module (ice-9 match))

#|
This is a short "parser" "combinator" library which
uses readers: essentially functions which take an optional
input port and return something to be read.
|#

(define-syntax reader
  (syntax-rules ()
    [(reader body body* ...)
     (lambda* (#:optional src)
       (let ([fn (lambda () body body* ...)])
         (if src
             (with-input-from-port src fn)
             (fn))))]))

(export reader)

(define-public eof (with-input-from-string "" read-char))

;;; Parsing Functor

;; Functor mapping (fmap)
(define-public (map-reader fn other-reader)
  "Applies a function to the output of a reader"
  (reader
   (let ([res (other-reader)])
     (if (eof-object? res)
         eof
         (fn res)))))

;;; Parsing Monad

;; bind (>>=)
(define-public (read-binding first-reader . reader-functions)
  "Reads the first value, then provides it as an argument to the next readers"
  (map-reader
   (lambda (res)
     (if (null? reader-functions)
         res
         ((apply read-binding ((car reader-functions) res) (cdr reader-functions)))))
   first-reader))

;; return 
(define-public (read-return obj)
  "Reader which always returns an object"
  (reader obj))

;; sequence
(define-public (read-sequence . readers)
  "Takes a list of readers and returns a list of results, or EOF if one fails"
  (reader
   (let loop ([vs '()]
              [rs readers])
     (if (null? rs)
         (reverse! vs)
         (let ([nxt ((car rs))])
           (if (eof-object? nxt)
               nxt
               (loop (cons nxt vs) (cdr rs))))))))

;;; Parsing Alternative

;; alternative (<|>)
(define-public (read-either . other-readers)
  "Attempts each of the readers in order"
  (reader
   (let loop ([rs other-readers])
     (if (null? rs)
         eof
         (let ([nxt ((car rs))])
           (if (eof-object? nxt)
               (loop (cdr rs))
               nxt))))))

;; empty
(define-public read-nothing
  (reader eof))

;; some
(define-public (read-some other-reader)
  "Attempts to read zero or more elements with a reader"
  (read-either
   (map-reader
    (lambda (r) (cons r ((read-some other-reader))))
    other-reader)
   (read-return '())))

;; many
(define-public (read-many other-reader)
  "Attempts to read at least one element with a reader"
  (reader
   (define r ((read-some other-reader)))
   (if (null? r) eof r)))

;; no direct haskell equivalent
(define-public (read-trying . readers)
  "Reads using each reader sequentially, until one of them fails."
  (if (null? readers)
      (read-return '())
      (read-either
       (map-reader
        (lambda (res) (cons res ((apply read-trying (cdr readers)))))
        (car readers))
       (read-return '()))))

;;; Matching constructs

(define-public (read-matching obj)
  "Reads a char matching the given object"
  (define (matches? thing)
    (cond [(eof-object? thing) #f]
          [(char-set? obj)     (char-set-contains? obj thing)]
          [(procedure? obj)    (obj thing)]
          [(char? obj)         (char=? obj thing)]
          [else                #f]))
  (reader
   (if (matches? (peek-char))
       (read-char)
       eof)))
