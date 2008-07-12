#lang scheme

(require "libraries/search/astar.ss")
(require "remember.scm")

; The idea is to store a "planned path", i.e. the path we will follow if no
; information arrives, and to recompute it whenever the planned path turns out
; to be blocked.

; Disregarding acceleration, an optimal path around an obstacle course is
; always a combination of straight lines tangential to an obstacle, and
; arcs around an obstacle. I think we can make the simplifying assumption
; that this is true even with acceleration.

; Given that, paths are compactly stored as a list of relevant obstacles that
; are circled around, plus the direction in which we avoid the obstacles.
; The empty list corresponds to the path that goes directly to the end point.

(provide make-path-node path-node? path-node-obstacle path-node-direction
         planned-path update-path)

; direction is either 1 if we pass to the left of the obstacle, -1 if to the
; right.
(define-struct path-node (obstacle direction))

(define planned-path (list))

(define (update-path obstacle)
  ; TODO: check if obstacle blocks our path, and if yes, recompute shortest
  ; path
  (printf "got obstacle ~n")
  (set! planned-path (cons (make-path-node obstacle -1) planned-path)))
