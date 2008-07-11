#lang scheme

(provide make-alpha-beta)
(require scheme/match)

(define (make-alpha-beta end-score heuristic-score current-player-goal generate-moves!)
  
  (define (evaluate state depth lower-bound upper-bound)
    (cache state depth lower-bound upper-bound
           (lambda ()
             (or (end-score state)
                 (and (<= depth 0)
                      (heuristic-score state))
                 (let-values (((score move next-state) (expand state depth lower-bound upper-bound)))
                   score)))))

  (define cache
    (let ((cache-table (make-hash)))
      (lambda (state depth lower-bound upper-bound thunk)
        (define (recompute)
          (let ((value (thunk)))
            (hash-set! cache-table state (vector depth lower-bound upper-bound value))
            value))
        (match (hash-ref cache-table state #f)
          (#f (recompute))
          ((vector old-depth old-lower-bound old-upper-bound old-value)
           (if (and (<= depth old-depth)
                    (>= lower-bound old-lower-bound)
                    (<= upper-bound old-upper-bound))
               old-value
               (recompute)))))))
  
  (define (expand state depth lower-bound upper-bound)
    (let ((best-move #f)
          (best-next-state #f))
      (let ((goal (current-player-goal state)))
        (let/ec break
          (generate-moves! state
                           (lambda (cost move next-state)
                             (let ((score (evaluate next-state (- depth cost)
                                                    lower-bound upper-bound)))
                               (case goal
                                 ('max (when (> score lower-bound)
                                         (set! lower-bound score)
                                         (set! best-move       move)
                                         (set! best-next-state next-state)))
                                 ('min (when (< score upper-bound)
                                         (set! upper-bound score)
                                         (set! best-move       move)
                                         (set! best-next-state next-state))))
                               (when (> lower-bound upper-bound)
                                 (break))))))
        (values (case goal ('max lower-bound) ('min upper-bound))
                best-move
                best-next-state))))
  
  (define (optimal-move state depth)
    (let-values (((score move next-state) (expand state depth -10000 10000)))
      (values move next-state)))

  optimal-move)
