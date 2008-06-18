#lang scheme

(require (prefix-in prio: "priority-queue.ss"))
(require "hash-util.ss")

(provide a*)

(define *heuristic-fn*      (make-parameter #f))
(define *heuristic-hash*    (make-parameter #f))
(define *depth-hash*        (make-parameter #f))
(define *frontier*          (make-parameter #f))
(define *goal-reached?-fn*  (make-parameter #f))
(define *possible-moves-fn* (make-parameter #f))
(define *make-move-fn*      (make-parameter #f))
(define *on-success-fn*     (make-parameter #f))


(define (heuristic state)
  (hash-memo (*heuristic-hash*)
             (*heuristic-fn*)
             state))

(define move-cost car)

(define (possible-moves state)
  ((*possible-moves-fn*) state))

(define (make-move state move)
  ((*make-move-fn*) state move))

(define (depth-ref state default)
  (hash-ref (*depth-hash*) state default))

(define (depth-set! state depth)
  (hash-set! (*depth-hash*) state depth))

(define (add! depth state path)
  (let ((h (heuristic state)))
    (depth-set! state depth)
    (prio:insert! (*frontier*)
                  (+ depth h)
                  (list depth state path))))

(define (frontier-empty?)
  (prio:empty? (*frontier*)))

(define (extract-min!)
  (prio:extract-min! (*frontier*)))

(define (maybe-add! depth state path)
  (let ((previous-depth (depth-ref state #f)))
    (when (or (not previous-depth)
              (< depth previous-depth))
      (add! depth state path))))

(define (success! path)
  ((*on-success-fn*) path))

(define (check-goal! state path)
  (when ((*goal-reached?-fn*) state)
    (success! path)))

(define (expand-a-state!)
  (let* ((item  (extract-min!))
         (depth (cadr item))
         (state (caddr item))
         (path  (cadddr item)))
    (check-goal! state path)
    (hash-remove! (*heuristic-hash*) state) ; save a little memory since it won't be needed any more
    ; (depth-set! state 0)                    ; save a little memory in case of rationals/bignums
    (for-each (lambda (move)
                (let ((next-state (make-move state move)))
                  (maybe-add! (+ depth (move-cost move))
                              next-state
                              (cons move path))))
              (possible-moves state))))

(define (a* init-state
            goal-reached?-fn
            heuristic-fn
            possible-moves-fn
            make-move-fn
            on-success-fn)
  (parameterize ((*heuristic-fn*      heuristic-fn)
                 (*goal-reached?-fn*  goal-reached?-fn)
                 (*possible-moves-fn* possible-moves-fn)
                 (*make-move-fn*      make-move-fn)
                 (*on-success-fn*     on-success-fn)
                 (*depth-hash*        (make-hash))
                 (*heuristic-hash*    (make-hash))
                 (*frontier*          (prio:make <)))
    (add! 0 init-state '())
    (let loop ()
      (unless (frontier-empty?)
        (expand-a-state!)
        (loop)))))