#lang scheme

(provide insert! make ref set!)

(define-struct cache (max (i #:mutable) values hash) #:transparent)

;Helper
(define (hash-exists? h key)
  (let/ec k (hash-ref h key (lambda () (k #f))) #t))

;Create a key/value pair in the cache
;Deletes another pair if it's over the size limit
(define (insert! c key value)
  (let ((h (cache-hash c))
        (v (cache-values c))
        (i (cache-i c)))
    (hash-set! h key value)
    (when (vector-ref v i)
      (hash-remove! h (unbox (vector-ref v i))))
    (vector-set! v i (box-immutable key))
    (set-cache-i! c (if (= (add1 i) (cache-max c)) 0 (add1 i)))))

;Make a cache of size max
(define (make max)
  (make-cache max 0 (make-vector max #f) (make-hash)))

;Wrap hash-ref
(define (ref c . rest)
  (apply hash-ref (cache-hash c) rest))

;Change or create a key/value pair in the cache
;Deletes another pair if it's over the size limit
(define (set! c key value)
  (let ((h (cache-hash c)))
    (if (hash-exists? h key)
        (hash-set! h key value)
        (insert! c key value))))
