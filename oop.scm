(define-module (shorthand oop)
  #:use-module (shorthand syntax)
  #:use-module (shorthand lists)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:export (define-struct define-children define-child-structs))

#|
The idea of define-struct is to define what in C++
are called PODs, or Plain Old Data classes. The gist of it
is that all of the fields have accessors, initial values and initial keywords.
|#

(define-syntax define-struct
  (lambda (stx)
    (syntax-case stx ()
      [(define-struct name fields ...)
       (identifier? #'name)
       #'(define-struct (name) fields ...)]
      [(define-struct (name parent ...) fields ...)
       #`(define-class name (parent ...)
           #,@(map shorthand-slot #'(fields ...)))])))

(define (shorthand-slot slot)
  (define (add-option options option value)
    (if (find (lambda (x) (eqv? (syntax->datum (car x)) option)) options)
        slot
        (cons (cons option value) options)))
  (if (identifier? slot)
      (shorthand-slot (list slot))
      (let* [(slot-data (syntax->list slot))
             (slot-name-stx (car slot-data))
             (slot-opts (klist->alist (cdr slot-data)))
             (slot-init-kw
              (add-option slot-opts #'#:init-keyword
                          (map-syntax symbol->keyword slot-name-stx)))
             (slot-init-ac
              (add-option slot-init-kw #'#:accessor slot-name-stx))]
        (cons slot-name-stx (alist->klist slot-init-ac)))))

#|
Utility for defining one-depth type hierarchies that mimic ML sum-types.
Specifically, defines a series of child structs stemming from a parent.
|#

(define-syntax define-children
  (lambda (stx)
    (syntax-rules ()
      [(define-children super)
       (begin)]
      [(define-children super (name slot ...) ...)
       (begin (define-class name (super) slot ...)
              ...)]
      [(define-children super name rest ...)
       (define-children super (name) rest ...)])))

(define-syntax define-child-structs
  (syntax-rules ()
    [(define-child-structs super)
     (begin)]
    [(define-child-structs super (name slot ...) rest ...)
     (begin (define-struct (name super) slot ...)
            (define-child-structs super rest ...))]
    [(define-child-structs super name rest ...)
     (define-child-structs super (name) rest ...)]))
