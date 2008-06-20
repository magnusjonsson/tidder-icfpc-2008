#lang scheme

(require scheme/foreign) (unsafe!)
(provide foo native-bar sum-vector sum-float-vector
         square-float-vector square-float-vector-smarter
         sum-and-reverse-vector)

(define (foo) "Hello World!")

(define native-lib (ffi-lib "native"))

(define native-bar (get-ffi-obj "bar" native-lib (_fun -> _int)))

(define sum-vector
  (get-ffi-obj "sum_array" native-lib
               (_fun (v) :: ((_vector i _int) = v) (_int = (vector-length v))
                             -> _int)))

(define sum-float-vector
  (get-ffi-obj "sum_float_array" native-lib
               (_fun (v : (_vector i _float))
                     (_int = (vector-length v))
                     -> _double)))


;; You can declare a function interface in a raw way
;; and provide your own wrapper:

(define native-square-float-vector!
  (get-ffi-obj "square_array_inplace" native-lib
               (_fun _pointer _int -> _void)))

(define (square-float-vector v)
  (let ((block (vector->cblock v _float))
        (n (vector-length v)))
    (native-square-float-vector! block n)
    (cblock->vector block _float n)))

;; Or you can do it in a more automatic way:

(define square-float-vector-smarter
  (get-ffi-obj "square_array_inplace" native-lib
               (_fun (v) :: (squared : (_vector io _float (vector-length v)) = v)
                            (_int = (vector-length v))
                            -> _void -> squared)))


(define sum-and-reverse-vector
  (get-ffi-obj "sum_rev" native-lib
               (_fun (v) :: (rev : (_vector io _int (vector-length v)) = v)
                            (_int = (vector-length v))
                            -> (sum : _int) -> (list sum rev))))

