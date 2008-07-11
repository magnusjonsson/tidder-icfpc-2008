#lang scheme

;;;Binary heap implementation
(provide make insert! remove! empty?)

;;Extensible vectors combine fast random access with automatic memory management
(require "../../../libraries/scheme/evector/evector.scm")

;;Supporting functions and data structures
(define-struct node (priority value) #:transparent)
(define-struct bheap (>? e))

(define (higher? b i this)
  ((bheap->? b)
   (node-priority (evector-ref (bheap-e b) i))
   (node-priority this)))

(define (evector-copy! e i j)
  (evector-set! e j (evector-ref e i)))

;;Actual implementation
(define (make >?) (make-bheap >? (evector)))
(define (empty? b) (zero? (evector-length (bheap-e b))))

;move things down until we find the right spot
(define (insert! b priority item)
  (let ((e (bheap-e b)) (this (make-node priority item)))
    (let bubble-loop ((i (evector-push! e #f)))
      (let ((parent (quotient (- i 1) 2)))
        (if (or (zero? i) (higher? b parent this)) ;root or right order?
            (evector-set! e i this)
            (begin (evector-copy! e parent i) (bubble-loop parent)))))))

;remove first, move last to first, bubble that down and return the removed
(define (remove! b)
  (let* ((e (bheap-e b))
         (removed (node-value (evector-ref e 0)))
         (last (sub1 (evector-length e))))
    (let loop ((i 0))
      (do ((child (+ (* 2 i) 1) (add1 child))
           (count 2 (sub1 count))
           (high last (if (higher? b child (evector-ref e high)) child high)))
        ((or (zero? count) (> child last)) ;no more children or out of bounds?
         (evector-copy! e high i)
         (if (= high last)
             (begin (evector-pop! e) removed)
             (loop high)))))))
