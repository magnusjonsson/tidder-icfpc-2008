#lang scheme

(provide a*)
(require (prefix-in queue- "bheap.scm"))

(define-struct state (g  path) #:transparent)

(define (a* start goal? succesors h)
  (let ((q (queue-make <))
        (closed (make-hash)))
    (queue-insert! q 1 (make-state 0 (list start)))
    (hash-set! closed start #t)

    (let loop ()
      (if (queue-empty? q)
          'fail
          (let* ((s (queue-remove! q))
                 (g (state-g s))
                 (p (state-path s))
                 (i (car p)))
            (if (goal? i)
                (reverse p)
                (begin
                  (for ((n (succesors i)))
                    (unless (hash-ref closed n #f)
                      (queue-insert! q (+ g (h n)) (make-state (add1 g) (cons n p)))
                      (hash-set! closed n #t)))
                  (loop))))))))
