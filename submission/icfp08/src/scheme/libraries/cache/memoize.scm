#lang scheme

(provide memoize)
(require (prefix-in cache- "fifo-bump-cache.scm")
         (only-in rnrs/base-6 assert))

(define (memoize size f)
  (let ((cache (cache-make size)))
    (lambda x
      (cache-ref cache x
                 (lambda ()
                   (let ((result (apply f x)))
                     (cache-insert! cache x result)
                     result))))))

(define (test)
  (assert (= (time (fib 1000))
             (time (fib 1000))
             70330367711422815821835254877183549770181269836358732742604905087154537118196933579742249494562611733487750449241765991088186363265450223647106012053374121273867339111198139373125598767690091902245245323403501
             )))

(define fib
  (memoize 10
           (lambda (x)
             (if (< x 2)
                 1
                 (+ (fib (- x 1)) (fib (- x 2)))))))
