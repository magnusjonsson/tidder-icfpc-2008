#lang scheme

(require scheme/foreign)
(unsafe!)

(provide foo)
(define (foo) "Hello World!")

(define native-lib (ffi-lib "native.so"))
(define native-bar (get-ffi-obj "bar" native-lib (_fun -> _int)))
(provide native-bar)

(define sum-vector
  (get-ffi-obj "sum_array" native-lib
               (_fun (v : (_vector i _int)) ;; first argument (call it v), an int vector
                     (_int = (vector-length v)) ;; second argument is the length of the vector
                     -> _int)))
(provide sum-vector)

(define sum-float-vector
  (get-ffi-obj "sum_float_array" native-lib
               (_fun (v : (_vector i _float))
                     (_int = (vector-length v))
                     -> _double)))

(provide sum-float-vector)

;; This one I could not get to work using the fancy _fun syntax,
;; so I wrapped it manually.
(define native-square-float-vector!
  (get-ffi-obj "square_array_inplace" native-lib
               (_fun _pointer
                     _int
                     -> _void)))
(define (square-float-vector v)
  (let ((block (vector->cblock v _float))
        (n (vector-length v)))
    (native-square-float-vector! block n)
    (cblock->vector block _float n)))

(provide square-float-vector)