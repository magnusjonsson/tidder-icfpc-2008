#lang scheme

(require "hash-util.ss")

(provide ida*)

(define (ida* start
              goal?
              heuristic
              generate-moves!
              success!)
  (let ((depth-hash     (make-hash))
        (heuristic-hash (make-hash))
        (max-depth      0)
        (next-max-depth 0))

    (define (heuristic* state)
      (hash-memo heuristic-hash heuristic state))
    
    (define (learn-heuristic! state distance)
      (when (> distance (heuristic* state))
        (hash-set! heuristic-hash state distance)))
    
    ;; returns a lower bound on the depth of the goal
    ;; side effect: when goal is found, success! is called with the path to the goal
    (define (depth-first-search state depth path)
      (let ((estimate (+ depth (heuristic* state))))
        (cond
          [(> estimate max-depth)
           (when (or (not next-max-depth) (< estimate next-max-depth))
             (set! next-max-depth estimate))
           estimate]

          [(goal? state)
           (success! path)
           depth]
          
          [(let ((best-depth (hash-ref depth-hash state #f)))
             (and best-depth (> depth best-depth)))
           estimate]

          [#t
           (hash-set! depth-hash state depth)
           (let ((new-estimate #f))
             (generate-moves! state
                              (lambda (cost desc next-state)
                                (let ((child-estimate
                                       (depth-first-search next-state
                                                           (+ depth cost)
                                                           (cons desc path))))
                                  (when (or (not new-estimate)
                                            (< child-estimate new-estimate))
                                    (set! new-estimate child-estimate)))))
             (if (and new-estimate (> new-estimate estimate))
                 (begin (learn-heuristic! state (- new-estimate depth))
                        new-estimate)
                 estimate))])))

    (let loop ()
      (when next-max-depth
        (set! max-depth next-max-depth)
        (set! next-max-depth #f)
        (printf "searching depth: ~a~n" max-depth)
        (depth-first-search start 0 '())
        (loop)))))