#lang scheme

(provide make set! ref)

(define-struct cache (max (i #:mutable) values hash) #:transparent)

;Helper
(define (hash-exists? h key)
  (call/cc (lambda (k) (hash-ref h key (lambda () (k #f))) #t)))

;Make a cache of size max
(define (make max) (make-cache max 0 (make-vector max #f) (make-hash)))

;Change or create a key/value pair in the cache
;Deletes another pair if it's over the size limit
(define (set! c key value)
  (let ((h (cache-hash c))
        (v (cache-values c))
        (i (cache-i c)))
    (if (hash-exists? h key)
        (hash-set! h key value)
        (begin
          (hash-set! h key value)
          (when (vector-ref v i)
            (hash-remove! h (unbox (vector-ref v i))))
          (vector-set! v i (box-immutable key))
          (set-cache-i! c (if (= (add1 i) (cache-max c)) 0 (add1 i)))))))

;Wrap hash-ref
(define (ref c . rest)
  (apply hash-ref (cache-hash c) rest))
