#lang scheme

(provide insert! make ref set!)

(define-struct cache (max (i #:mutable) values marks hash) #:transparent)

;Helpers
(define (hash-exists? h key)
  (let/ec k (hash-ref h key (lambda () (k #f))) #t))

(define (next-i c i)
  (let ((next (add1 i)))
    (if (= next (cache-max c)) 0 next)))

(define (add-mark! m key x)
  (hash-set! m key (+ x (hash-ref m key 0))))

;Create a key/value pair in the cache
;Deletes another pair if it's over the size limit
(define (insert! c key value)
  (let ((h (cache-hash c))
        (v (cache-values c))
        (m (cache-marks c)))
    ;Try to find an unmarked spot
    (do ((bkey (vector-ref v (cache-i c)) (vector-ref v i))
         (i (cache-i c) (next-i c i)))
      ((or (not bkey) (not (hash-ref m (unbox bkey) #f)))
       ;When we found a spot
       (when (vector-ref v i)
         (hash-remove! h (unbox (vector-ref v i)))
         (hash-remove! m (unbox (vector-ref v i))))
       (vector-set! v i (box-immutable key))
       (hash-set! h key value)
       (set-cache-i! c (next-i c i)))
      (hash-set! m (unbox bkey) #f))))

;Make a cache of size max
(define (make max)
  (make-cache max 0 (make-vector max #f) (make-hash) (make-hash)))

;Wrap hash-ref
(define (ref c key . rest)
  (hash-set! (cache-marks c) key #t)
  (apply hash-ref (cache-hash c) key rest))

;Change or create a key/value pair in the cache
;Deletes another pair if it's over the size limit
(define (set! c key value)
  (let ((h (cache-hash c)))
    (if (hash-exists? h key)
        (hash-set! h key value)
        (insert! c key value))))
