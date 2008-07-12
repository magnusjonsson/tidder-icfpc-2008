#lang scheme

(provide insert! make ref set!)

(define-struct cache (hash size first-node last-node) #:mutable #:transparent)
(define-struct node  (blink key value flink) #:mutable #:transparent)

;Helper
(define (cache-exists? c key)
  (let/ec k (hash-ref (cache-hash c) key (lambda () (k #f))) #t))

;Remove an item from the cache
(define (del! c key)
  (let ((n (hash-ref (cache-hash c) key)))
    (hash-remove! (cache-hash c) key)
    
    (cond ((node-blink n) => (lambda (prev)
                               (set-node-flink! prev (node-flink n)))))
    (cond ((node-flink n) => (lambda (next)
                               (set-node-blink! next (node-blink n)))))
    
    (when (eq? (cache-first-node c) n)
      (set-cache-first-node! c (node-flink n)))
    (when (eq? (cache-last-node c) n)
      (set-cache-last-node! c (node-blink n)))))


;Insert an item at the end
;Will screw things up if the same key used already
;so use set! if there is any chance of this happening
(define (insert! c key value)
  ;Remove one when we go over the limit
  (when (>= (hash-count (cache-hash c)) (cache-size c))
    (del! c (node-key (cache-first-node c))))
  (noremove-insert! c key value))

;Insert! without deleting anything
(define (noremove-insert! c key value)
  (let ((n (make-node (cache-last-node c) key value #f)))
    (hash-set! (cache-hash c) key n)
    (set-cache-last-node! c n)
    (cond ((node-blink n) => (lambda (prev)
                               (set-node-flink! prev n))))
    (unless (cache-first-node c)
      (set-cache-first-node! c n))))

;Get an item, else return def or call it if it's is a procedure
;Moves the found item on top of the fifo stack again
(define (ref c key def)
  (let ((n (hash-ref (cache-hash c) key #f)))
    (if (not n)
        (if (procedure? def) (def) def)
        (begin
          (unless (eq? (cache-last-node c) n)
            (del! c key)
            (insert! c key (node-value n)))
          (node-value n)))))

;Constructor
(define (make size)
  (make-cache (make-hash) size #f #f))

;Destructively update or create
;Safe but kinda unoptimized
(define (set! c key value)
  (when (cache-exists? c key) (del! c key))
  (insert! c key value))
