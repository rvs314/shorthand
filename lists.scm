(define-module (shorthand lists)
  #:use-module (guile)
  #:use-module (srfi srfi-1)
  #:use-module (shorthand functions))

(define-public (klist->alist lst)
  "Turns a keyword list into an alist: (a 1 b 2) → ((a . 1) (b . 2))"
  (let loop ((lst lst) (a '()))
    (cond [(null? lst) (reverse! a)]
          [(null? (cdr lst)) (reverse! (cons (car lst) a))]
          [else (loop (cddr lst) (cons (cons (car lst) (cadr lst)) a))])))

(define-public (alist->klist lst)
  "Turns an alist into a keyword list: ((a . 1) (b . 2)) → (a 1 b 2)"
  (fold-right (lambda (k a)
                (if (pair? k)
                    (cons (car k) (cons (cdr k) a))
                    (cons k a)))
              '() lst))

(define-public (map-index f l . ls)
  "Maps a function over the elements and indicies of a list"
  ;; This can be made more efficient by not allocating an iota list,
  ;; but it's probably fine
  (apply map f (iota (length l) 0) l ls))

(define-public (for-each-index f l . ls)
  "Applies a function over the elements and indicies of a list: returns nothing"
  ;; See above
  (apply for-each f (iota (length l) 0) l ls))

(export push!)

(define-syntax-rule (push! obj loc)
  (set! loc (cons obj loc)))

