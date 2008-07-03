#lang mzscheme

(provide make push! pop! empty? length)

(require (only scheme/base vector-copy!))
(require (only rnrs/base-6 assert))

(define-struct fifo (read-pos length vector))

(define (make)
  (make-fifo 0 0 (make-vector 16)))

(define (push! q item)
  (ensure-space! q)
  (let ((v (fifo-vector q))
        (n (fifo-length q)))
    (vector-set! v
                 (bitwise-and (+ (fifo-read-pos q) n)
                              (sub1 (vector-length v)))
                 item)
    (set-fifo-length! q (add1 n))))

(define (pop! q)
  (when (empty? q)
    (raise 'queue-empty))
  (let ((v (fifo-vector q))
        (p (fifo-read-pos q)))
    (begin0
      (vector-ref v p)
      (vector-set! v p #f) ; allow garbage collection
      (set-fifo-read-pos! q (bitwise-and (add1 p) (sub1 (vector-length v))))
      (set-fifo-length! q (sub1 (fifo-length q))))))

(define (ensure-space! q)
  (when (full? q)
    (grow! q)))

(define (full? q)
  (= (fifo-length q) (vector-length (fifo-vector q))))

(define (grow! q)
  (define old-vec (fifo-vector q))
  (define old-len (vector-length old-vec))
  (define new-len (* old-len 2))
  (define new-vec (make-vector new-len #f))
  
  (let ((read-pos (fifo-read-pos q))
        (count (fifo-length q)))
    (define n (min count (- old-len read-pos)))
    (vector-copy! new-vec 0
                  old-vec read-pos
                  (+ read-pos n))
    (unless (= n count)
      (vector-copy! new-vec n
                    old-vec 0
                    (- count n))))
  (set-fifo-vector! q new-vec)
  (set-fifo-read-pos! q 0))

(define (empty? q)
  (zero? (fifo-length q)))

(define (length q)
  (fifo-length q))



(define (test)
  (define q (make))
  (let loop ((pushes 0)
             (pops 0))
    (assert (= (length q) (- pushes pops)))
    (when (< pushes 1000000)
      (if (or (= pops pushes) (< (random) 0.5))
          (begin
            (push! q pushes)
            (loop (add1 pushes) pops))
          (begin
            (assert (= (pop! q) pops))
            (loop pushes (add1 pops)))))))
                  