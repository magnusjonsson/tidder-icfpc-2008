#lang scheme

(provide make insert! set! ref)

(define-struct cache (max values hash) #:transparent)

;Helper
(define (hash-exists? h key)
  (let/ec k (hash-ref h key (lambda () (k #f))) #t))

;Make a cache of size max
(define (make max) (make-cache max (make-vector max) (make-hash)))

;Insert a key/value pair without checking for duplicates
;Warning: inserting duplicates this way is harmful
(define (insert! c key value)
  (let* ((h (cache-hash c)) (v (cache-values c)) (size (hash-count h)))
    (if (< size (cache-max c))
        (begin (hash-set! h key value) (vector-set! v size key))
        (let ((replaced (random (cache-max c))))
          (hash-remove! h (vector-ref v replaced))
          (hash-set! h key value)
          (vector-set! v replaced key)))))

;Change or create a key/value pair in the cache
;Deletes another pair if it's over the size limit
(define (set! c key value)
  (let ((h (cache-hash c)) (v (cache-values c)))
    (if (hash-exists? h key)
        (hash-set! h key value)
        (insert! c key value))))

;Wrap hash-ref
(define (ref c . rest)
  (apply hash-ref (cache-hash c) rest))
