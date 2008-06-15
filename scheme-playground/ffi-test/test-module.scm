#lang scheme

(require scheme/foreign) (unsafe!)
(provide foo native-bar sum-vector sum-and-reverse-vector)

(define (foo) "Hello World!")

(define native-lib (ffi-lib "native"))

(define native-bar (get-ffi-obj "bar" native-lib (_fun -> _int)))

(define sum-vector
  (get-ffi-obj "sum_array" native-lib
               (_fun (v) :: ((_vector i _int) = v) (_int = (vector-length v))
                             -> _int)))

(define sum-and-reverse-vector
  (get-ffi-obj "sum_rev" native-lib
               (_fun (v) :: (rev : (_vector io _int (vector-length v)) = v)
                            (_int = (vector-length v))
                            -> (sum : _int) -> (list sum rev))))
