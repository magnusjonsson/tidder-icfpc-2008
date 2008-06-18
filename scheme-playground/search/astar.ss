#lang scheme

(require (prefix-in prio: "priority-queue.ss"))
(require "hash-util.ss")

(provide a*)

(define-struct item (prio depth state path))

(define (a* init-state
            goal-reached?-fn
            heuristic-fn
            possible-moves-fn
            make-move-fn
            on-success-fn)
  (define depth-hash     (make-hash))
  (define heuristic-hash (make-hash))
  (define (item-< item1 item2)
    (< (item-prio item1)
       (item-prio item2)))
  (define frontier       (prio:make item-<))
  (define move-cost      car)

  (define (add! depth state path)
    (let ((prev-depth (hash-ref depth-hash state #f)))
      (when (or (not prev-depth)
                (< depth prev-depth))
        (let ((h (hash-memo heuristic-hash
                            heuristic-fn
                            state))) 
          (hash-set! depth-hash state depth)
          (prio:insert! frontier (make-item (+ depth h) depth state path))))))
  
  (add! 0 init-state '())
  
  (let loop ()
    (unless (prio:empty? frontier)
      (let* ((item  (prio:extract-min! frontier))
             (depth (item-depth item))
             (state (item-state item))
             (path  (item-path item)))
        (when (goal-reached?-fn state)
          (on-success-fn path))
        (hash-remove! heuristic-hash state) ; save a little memory since it won't be needed any more
        ; (hash-set! depth-hash state 0)      ; save a little memory in case of rationals/bignums
        (for-each (lambda (move)
                    (let ((next-state (make-move-fn state move)))
                      (add! (+ depth (move-cost move))
                            next-state
                            (cons move path))))
                  (possible-moves-fn state)))
      (loop))))