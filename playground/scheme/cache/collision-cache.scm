#lang scheme

(provide make insert! set! ref)

(define-struct cache (size vector) #:transparent)

(define (make size) (make-cache size (make-vector size #f)))

(define (insert! c key value)
  (let ((hash (equal-hash-code key)))
    (vector-set! (cache-vector c)
                 (modulo hash (cache-size c))
                 (cons hash value))))

(define set! insert!)

(define (ref c key def)
  (let* ((hash (equal-hash-code key))
         (v (vector-ref (cache-vector c)
                        (modulo hash (cache-size c))))) 
    (if (and v (eq? (car v) hash))
        (cdr v)
        (if (procedure? def) (def) def))))
