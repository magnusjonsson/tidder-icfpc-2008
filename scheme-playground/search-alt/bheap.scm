#lang scheme

;;;Binary heap implementation
(provide make insert! remove! empty?)

;;Extensible vectors combine fast random access with automatic memory management
(require (planet "evector.scm" ("soegaard" "evector.plt" 1 1)))

;;Supporting functions and data structures
(define-struct node (priority value) #:transparent)
(define-struct bheap (>? e))

(define (higher? b e i j)
  ((bheap->? b)
   (node-priority (evector-ref e i))
   (node-priority (evector-ref e j))))

(define (evector-swap! e i j)
  (let ((e-i (evector-ref e i)))
    (evector-set! e i (evector-ref e j))
    (evector-set! e j e-i)))

;;Actual implementation
(define (make >?) (make-bheap >? (evector)))
(define (empty? b) (zero? (evector-length (bheap-e b))))

;insert at the end then bubble up
(define (insert! b priority item)
  (let ((e (bheap-e b)))
    (evector-push! e (make-node priority item))
    (let loop ((i (sub1 (evector-length e))))
      (let ((parent (quotient (- i 1) 2)))
        (unless (or (zero? i)               ;root?
                    (higher? b e parent i)) ;right order?
          (evector-swap! e i parent) (loop parent))))))

;remove first, move last to first, bubble that down and return the removed
(define (remove! b)
  (let ((e (bheap-e b)))
    (let ((removed (node-value (evector-ref e 0))))
      (evector-set! e 0 (evector-ref e (sub1 (evector-length e))))
      (evector-pop! e)
      (let loop ((i 0))
        (do ((child (+ (* 2 i) 1) (add1 child))
             (count 2 (sub1 count))
             (highest i (if (higher? b e child highest) child highest)))
          ((or (zero? count)                   ;no more children
               (>= child (evector-length e)))  ;out of bounds
           (if (= i highest)
               removed
               (begin (evector-swap! e i highest) (loop highest)))))))))
