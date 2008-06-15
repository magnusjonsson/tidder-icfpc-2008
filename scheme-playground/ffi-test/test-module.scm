#lang scheme

(require scheme/foreign)
(unsafe!)

(provide foo)
(define (foo) "Hello World!")

(define native-lib (ffi-lib "native.so"))
(define native-bar (get-ffi-obj "bar" native-lib (_fun -> _int)))
(provide native-bar)


(define native-sum-array
  (get-ffi-obj "sum_array" native-lib
               (_fun (_vector i _int) _int -> _int)))

(define sum-vector
  (lambda (vector)
    (native-sum-array vector (vector-length vector))))

(provide sum-vector)