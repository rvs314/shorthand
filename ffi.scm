(define-module (shorthand ffi)
  #:use-module (shorthand oop)
  #:use-module (shorthand syntax)
  #:use-module (shorthand functions)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:duplicates (merge-generics)
  #:export (->pointer ->pointer!))

(define-syntax define-ffi
  (lambda (stx)
    (syntax-case stx ()
      [(_ dyn-lib) #f]
      [(_ dyn-lib (name (ret-type c-name (args ...))) rest ...)
       (let [(raw-pointer-name (map-syntax (lambda (s) (string->symbol (format #f "$~a" s)))
                                           #'name))]
         #`(begin
             (define-public #,raw-pointer-name
                 (foreign-library-pointer
                   dyn-lib
                   #,(symbol->string (syntax->datum #'c-name))))
                                          
             (define-public name
               (pointer->procedure
                  ret-type #,raw-pointer-name
                  (list args ...))) 
  
             (define-ffi dyn-lib rest ...)))])))

(export-syntax define-ffi)

(define-public ptr '*)

(define-public NULL %null-pointer)

(define-syntax-rule (assert-not-null value)
  (let [(v value)]
    (if (and (pointer? v) (null-pointer? v))
        (assertion-violation 'assert-not-null "Expected non-null value" 'value)
        v)))

(export-syntax assert-not-null)

(define-method (->pointer (obj <top>))
  (if (pointer? obj)
      obj
      NULL))

(define-method (->pointer (obj <boolean>))
  NULL)

(define-method (->pointer (obj <integer>))
  ;; TODO: Make this not a magic number
  (define MAX-POINTER 18446744073709551615)
  (if (and (not (negative? obj)) (<= obj MAX-POINTER))
      (make-pointer obj)
      NULL))

(define-method (->pointer (obj <string>))
  (string->pointer obj))

(define-syntax-rule (->pointer! obj ...)
  (begin (set! obj (->pointer obj))
         ...))

(define-struct <wrapped-pointer> pointer)

(define-method (->pointer (obj <wrapped-pointer>))
  (pointer obj))

(define-method (write (obj <wrapped-pointer>) port)
  (let ((p (pointer obj)))
    (format port "#<~a at 0x~x>"
            (class-name (class-of obj))
            (pointer-address p))))

(export <wrapped-pointer> pointer
        write)

(define-public (allocate-array type . items)
  "Allocates several items of 'type' lined up consecutively"
  (values (make-c-struct (map (lambda _ type) items)
                         items)
          (length items)))

(export make-array)

(define-public (pointer-with-finalizer ptr finalizer)
  "Returns the pointer object ptr, but with a C function which is called when
   the pointer object goes out of scope"
  (make-pointer (pointer-address ptr) finalizer))

(define-public (boolean->integer x)
  "Converts boolean values into integers"
  (if x 1 0))

(define-public (integer->boolean x)
  "Converts an integer into a boolean"
  (not (zero? x)))

(define-struct (<blob> <wrapped-pointer>) size)
(export <blob> size)
