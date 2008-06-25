#lang scheme

(provide make-minimax)
(require scheme/match)

(define (make-minimax end-score heuristic-score current-player-goal generate-moves!)
  
  (define (evaluate state depth)
    (cache state depth
           (lambda ()
             (or (end-score state)
                 (and (<= depth 0)
                      (heuristic-score state))
                 (let-values (((score move next-state) (expand state depth)))
                   score)))))

  (define cache
    (let ((cache-table (make-hash)))
      (lambda (state depth thunk)
        (define (recompute)
          (let ((value (thunk)))
            (hash-set! cache-table state (cons depth value))
            value))
        (match (hash-ref cache-table state #f)
          (#f (recompute))
          ((cons old-depth old-value)
           (if (<= depth old-depth)
               old-value
               (recompute)))))))

  (define (expand state depth)
    (let ((best-score #f)
          (best-move #f)
          (best-next-state #f))
      (let ((goal (current-player-goal state)))
        (generate-moves! state
                         (lambda (cost move next-state)
                           (let ((score (evaluate next-state (- depth cost))))
                             (when (or (not best-score)
                                       (case goal
                                         ('max (< best-score score))
                                         ('min (> best-score score))))
                               (set! best-score      score)
                               (set! best-move       move)
                               (set! best-next-state next-state)))))
        (values best-score best-move best-next-state))))
  
  (define (optimal-move state depth)
    (let-values (((score move next-state) (expand state depth)))
      (values move next-state)))

  optimal-move)