#lang scheme

(require (prefix-in prio: "priority-queue.ss"))
(require "hash-util.ss")

(provide a*)

(define (a* init-state
            goal-reached?-fn
            heuristic-fn
            possible-moves-fn
            make-move-fn
            on-success-fn)
  (define depth-hash     (make-hash))
  (define heuristic-hash (make-hash))
  (define frontier       (prio:make <))

  (define (heuristic state)
    (hash-memo heuristic-hash
               heuristic-fn
               state))

  (define move-cost car)
 
  (define (add! depth state path)
    (let ((h (heuristic state)))
      (hash-set! depth-hash state depth)
      (prio:insert! frontier
                    (+ depth h)
                    (list depth state path))))
  
  (define (maybe-add! depth state path)
    (let ((prev-depth (hash-ref depth-hash state #f)))
      (when (or (not prev-depth)
                (< depth prev-depth))
        (add! depth state path))))
  
  (define (expand-a-state!)
    (let* ((item  (prio:extract-min! frontier))
           (depth (cadr item))
           (state (caddr item))
           (path  (cadddr item)))
      (when (goal-reached?-fn state)
        (on-success-fn path))
      (hash-remove! heuristic-hash state) ; save a little memory since it won't be needed any more
      ; (hash-set! depth-hash state 0)      ; save a little memory in case of rationals/bignums
      (for-each (lambda (move)
                  (let ((next-state (make-move-fn state move)))
                    (maybe-add! (+ depth (move-cost move))
                                next-state
                                (cons move path))))
                (possible-moves-fn state))))
  
  (add! 0 init-state '())
  
  (let loop ()
    (unless (prio:empty? frontier)
      (expand-a-state!)
      (loop))))