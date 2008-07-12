#lang scheme

(provide push! pop! make empty?)

(define-struct fifo (first last) #:mutable #:transparent)

(define (push! x q)
  (let ((new (mcons x '())))
    (if (empty? q)
        (begin (set-fifo-first! q new)
               (set-fifo-last! q new))
        (begin (set-mcdr! (fifo-last q) new)
               (set-fifo-last! q new)))))

(define (pop! q)
  (begin0 (mcar (fifo-first q))
          (if (null? (mcdr (fifo-first q)))
              (begin (set-fifo-first! q #f)
                     (set-fifo-last! q #f))
              (set-fifo-first! q (mcdr (fifo-first q))))))

(define (make)
  (make-fifo #f #f))

(define (empty? q)
  (not (fifo-first q)))
