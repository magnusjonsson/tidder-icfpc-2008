#lang scheme

(provide hash-memo)

(define (hash-memo hash f x)
  (hash-ref hash x
            (lambda ()
              (let ((fx (f x)))
                (hash-set! hash x fx)
                fx))))
