#lang scheme

(require "hash-util.ss")

(provide ida*)

(define *heuristic-fn*      (make-parameter #f))
(define *heuristic-hash*    (make-parameter #f))
(define *depth-hash*        (make-parameter #f))
(define *goal-reached?-fn*  (make-parameter #f))
(define *possible-moves-fn* (make-parameter #f))
(define *make-move-fn*      (make-parameter #f))
(define *max-depth*         (make-parameter #f))
(define *next-max-depth*    (make-parameter #f))
 
(define (heuristic state)
  (hash-memo (*heuristic-hash*)
             (*heuristic-fn*)
             state))

(define (learn-heuristic! state distance)
  (when (> distance (heuristic state))
    (hash-set! (*heuristic-hash*)
               state
               distance)))

(define (maybe-next-max-depth! depth)
  (when (or (not (*next-max-depth*))
            (< depth (*next-max-depth*)))
    (*next-max-depth* depth)))

(define move-cost car)

;; returns a lower bound on the depth of the goal
;; side effect: when goal is found, on-success is called with the path to the goal
(define (depth-first-rec state
                         depth
                         on-success)
  (let ((min-goal-depth (+ depth (heuristic state))))
    (cond
      [(> min-goal-depth (*max-depth*))
       (maybe-next-max-depth! min-goal-depth)
       min-goal-depth]
      [((*goal-reached?-fn*) state)
       (on-success '())
       depth]
      [(> depth (hash-ref (*depth-hash*) state (+ depth 1)))
       min-goal-depth]
      [#t
       (hash-set! (*depth-hash*) state depth)
       (let ((min-goal-depth-2 #f))
         (for-each (lambda (move)
                     (let ((child-depth
                            (depth-first-rec ((*make-move-fn*) state move)
                                             (+ depth (move-cost move))
                                             (lambda (success-path)
                                               (on-success (cons move success-path))))))
                       (when (or (not min-goal-depth-2)
                                 (< child-depth min-goal-depth-2))
                         (set! min-goal-depth-2 child-depth))))
                   ((*possible-moves-fn*) state))
         (if (and min-goal-depth-2
                  (> min-goal-depth-2 min-goal-depth))
             (begin
               (learn-heuristic! state (- min-goal-depth-2 depth))
               min-goal-depth-2)
             min-goal-depth))])))

(define (ida* init-state
              goal-reached?-fn
              heuristic-fn
              possible-moves-fn
              make-move-fn
              on-success)
  (parameterize ((*heuristic-fn*      heuristic-fn)
                 (*heuristic-hash*    (make-hash))
                 (*depth-hash*        (make-hash))
                 (*goal-reached?-fn*  goal-reached?-fn)
                 (*possible-moves-fn* possible-moves-fn)
                 (*make-move-fn*      make-move-fn)
                 (*max-depth*         0)
                 (*next-max-depth*    0))
    (let loop ()
      (when (*next-max-depth*)
        (*max-depth* (*next-max-depth*))
        (*next-max-depth* #f)
        (printf "searching depth: ~a~n" (*max-depth*))
        (depth-first-rec init-state 0 on-success)
        (loop)))))