#lang scheme

(require "libraries/search/astar.ss")
(require "vec2.scm")
(require "remember.scm")
(require "intersect.scm")
(require "messages.scm")
(require "tangent.scm")
(require (only-in rnrs/base-6 assert))
(require "misc-syntax.ss")
(require (prefix-in gfx- "graphics.scm"))
(require "libraries/cache/memoize.scm")

(provide safety-margin compute-target must-recompute-path draw-path)

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

(define safety-margin 2.0)

(define (safe-radius o)
  (+ safety-margin
     (obj-radius o)))

(define (old-compute-target pos)
  ; pos = position of our rover
  (safe-point pos (make-vec2 0 0) #f))

; Returns a point that we can safely travel to. For the full pathfinding,
; this needs to generate all suitable points.
(define (safe-point pos target ignore)
  (let ((real-target target)
        (visited (make-hash))
        (obstacle #f)
        (direction 0))
    (when ignore (hash-set! visited ignore #t))
    (let avoidance-loop ()
      (printf ".")
      (let* ((ray (vec2- target pos))
             (b (first-hit-obj pos ray)))
        (when (and b (not (hash-ref visited b #f)))
          (hash-set! visited b #t)
          (let ((t (ray-circle-intersection-first-time
                    pos ray (obj-pos b) (obj-radius b))))
            (when (< t 1)
              ; adjust target to be left tangent point
              ; this is the point where we will need to branch for the full
              ; search, and also consider the right tangent point.
              (set! target (tangent pos (obj-pos b) (safe-radius b) 1))
              (set! obstacle b)
              (set! direction 1)
              (avoidance-loop))))))
    (tangent-point pos real-target obstacle direction)))

; Finds a tangent point on the obstacle with the given direction, keeping an
; eye on the final target. If another obstacle is connected to this one and
; blocks the arc that we want to drive, considers finding a tangent point on
; that obstacle instead.
(define (tangent-point pos target obstacle direction)
  (if (not obstacle) target
      (let* ((p (tangent pos (obj-pos obstacle) (safe-radius obstacle) direction))
             (q (tangent target (obj-pos obstacle) (safe-radius obstacle) (- direction)))
             (arc (curve-angle p (obj-pos obstacle) q (- direction)))
             (arc-max (first-curve-hit-angle p (obj-pos obstacle) (- direction))))
        (printf "obstacle ~a~n" obstacle)
        (cond
          ((or (not arc-max) (<= arc arc-max)) p) ; "easy case"
          (else
           ; the curve around obstacle that we plan to drive is partially blocked
           ; so change your target to aim at the blocker instead
           (printf "blocker ~a~n" (first-curve-hit-obj obstacle p (obj-pos obstacle) (- direction)))
           (let* ((blocker (first-curve-hit-obj obstacle p (obj-pos obstacle) (- direction)))
                  (t (circle-circle-tangent (obj-pos obstacle) (safe-radius obstacle) direction
                                            (obj-pos blocker) (safe-radius blocker) direction))
                  (target-on-blocker (cdr t)))
             (safe-point pos target-on-blocker blocker)))))))


;
; Beginning of new A* based code
;

(define-struct arc (host-obj ccw-obj cw-obj) #:transparent)

(define (arc-contains-point arc point)
  ; is this point on the arc's host object inside the arc?
  ; todo: implement
  (define (hit dir obj)
    (curve-circle-intersection-angle2
     point (obj-pos (arc-host-obj arc)) (obj-radius (arc-host-obj arc)) dir
     (obj-pos obj) (obj-radius obj)))
  (or (not (arc-ccw-obj arc)) (not (arc-cw-obj arc))
      (let ((ccw-angle-to-ccw-obj (hit 1 (arc-ccw-obj arc)))
            (ccw-angle-to-cw-obj (hit 1 (arc-cw-obj arc)))
            (cw-angle-to-ccw-obj (hit -1 (arc-ccw-obj arc)))
            (cw-angle-to-cw-obj (hit -1 (arc-cw-obj arc))))
        ; check if cw-obj is hit first if we go cw, and if ccw-obj is hit first if
        ; we go ccw. if outside the arc, either the same object is hit twice, or
        ; they are hit in the wrong order, so the test fails.
        (and (<= ccw-angle-to-ccw-obj ccw-angle-to-cw-obj)
             (<= cw-angle-to-cw-obj cw-angle-to-ccw-obj)))))
  
(define (test-arc-contains-point)
  (define obj1 (make-obj 'boulder (make-vec2 0 0) 15))
  (define obj2 (make-obj 'boulder (make-vec2 10 0) 15))
  (define obj3 (make-obj 'boulder (make-vec2 20 0) 15))
  (define arc (make-arc obj2 obj1 obj3))
  (assert (arc-contains-point arc (make-vec2 10 15)))
  (assert (not (arc-contains-point arc (make-vec2 10 -15)))))

(define-struct directed-arc (arc direction) #:transparent)


(define (tangent-info->directed-arc tangent)
  (match tangent
    ((list obj dir tangent-point)
     (let* ((ccw-obj (first-curve-hit-obj obj tangent-point (obj-pos obj) 1))
            (cw-obj (first-curve-hit-obj obj tangent-point (obj-pos obj) -1))
            (arc (make-arc obj ccw-obj cw-obj)))
       (make-directed-arc arc dir)))))

(define (test-ti->da)
  (define obj1 (make-obj 'boulder (make-vec2 0 0) 15))
  (define obj2 (make-obj 'boulder (make-vec2 10 0) 15))
  (define obj3 (make-obj 'boulder (make-vec2 20 0) 15))
  (define tangent (list obj2 1 (make-vec2 10 15)))
  (clear-remembered)
  (remember-object obj1)
  (remember-object obj2)
  (remember-object obj3)
  (let ((da (tangent-info->directed-arc tangent)))
    (printf "~a~n" (tangent-info->directed-arc tangent))
    (assert (eq? (arc-ccw-obj (directed-arc-arc da)) obj1))
    (assert (eq? (arc-cw-obj (directed-arc-arc da)) obj3))))

(define (reachable-states state)
  (match state
    ((struct vec2 (0 0)) ; home
     (list)) ; don't go anywhere after coming home
    ((struct vec2 (_ _)) ; our rover
     (if (not (line-obstructed? state vec2-origin))
         ; if we can go home directly, don't even consider doing something else
         (list vec2-origin)
         ; otherwise find reachable directed arcs
         (map tangent-info->directed-arc
              (unobstructed-point-obj-tangents state))))
    ((struct directed-arc (arc direction))
     (let ((obj1 (arc-host-obj arc)))
       ; is home reachable from this arc?
       (if (let ((tangent-point (tangent vec2-origin
                                         (obj-pos obj1)
                                         (obj-radius obj1)
                                         direction)))
             (and
              (not (line-obstructed? tangent-point vec2-origin (list obj1)))
              (arc-contains-point arc tangent-point)))
           ; yes, so return it
           (list vec2-origin)
           ; otherwise find reachable directed arcs
           (map tangent-info->directed-arc
                (filter-map (lambda (tangent-info)
                              (match tangent-info
                                ((list tp1 obj2 dir2 tp2)
                                 (and (arc-contains-point arc tp1)
                                      (list obj2 dir2 tp2)))))
                     (unobstructed-obj-obj-tangents obj1 direction))))))))



(define (pretty-list-of-length correct-length list)
  (printf "list:~n")
  (dolist (i list)
          (printf "--> ~a~n" i))
  (if (= (length list) correct-length)
      (printf "length ok~n")
      (begin (printf "length is: ~a, should be ~a~n" (length list) correct-length)
             (assert #f))))

(define (test-circle-reachability)
  (clear-remembered)
  (let ((o1 (make-obj 'crater (make-vec2 0 10) 1))
        (o2 (make-obj 'crater (make-vec2 5 0) 1))
        (o3 (make-obj 'crater (make-vec2 0 5) 2)))
    (remember-object o1)
    (printf "should only consider going directly home~n")
    (pretty-list-of-length 1 (reachable-states (make-vec2 10 0)))
    (remember-object o2)
    (printf "two directions to walk around o2, two directions to walk around o1~n")
    (pretty-list-of-length 4 (reachable-states (make-vec2 10 0)))
    
    (printf "reachability from arcs~n")
    (printf "should go home:~n")
    (pretty-list-of-length 1 (reachable-states (make-directed-arc
                                                (make-arc o1 #f #f)
                                                1)))
    (pretty-list-of-length 1 (reachable-states (make-directed-arc
                                                (make-arc o1 #f #f)
                                                -1)))
    (printf "placing an obstructing object~n")
    (remember-object o3)
    (printf "reachability from arcs again:~n")
    (printf "two ways around o2, two ways around o3:~n")
    (pretty-list-of-length 4 (reachable-states (make-directed-arc
                                                (make-arc o1 #f #f)
                                                1)))
    (printf "one way around o2 (the other is obstructed by o3), two ways around o3:~n")
    (pretty-list-of-length 3 (reachable-states (make-directed-arc
                                                (make-arc o1 #f #f)
                                                -1)))
    ))

(define (test-arc-reachability)
  (clear-remembered)
  (let ((o1 (make-obj 'crater (make-vec2 -1 0) 2))
        (o2 (make-obj 'crater (make-vec2  1 0) 2)))
    (printf "two objs that overlap~n")
    (remember-objects (list o1 o2))
    (printf "from below~n")
    (pretty-list-of-length 2 (reachable-states (make-vec2 0 -10)))
    (printf "from above~n")
    (pretty-list-of-length 2 (reachable-states (make-vec2 0 10)))
    (printf "from the left~n")
    (pretty-list-of-length 2 (reachable-states (make-vec2 -10 0)))
    (printf "from the right~n")
    (pretty-list-of-length 2 (reachable-states (make-vec2 10 0)))
    ))

(define (test-arc-reachability2)
  (clear-remembered)
  (let ((o1 (make-obj 'crater (make-vec2 -1 2) 2))
        (o2 (make-obj 'crater (make-vec2  0 2) 2))
        (o3 (make-obj 'crater (make-vec2  1 2) 2))
        (o4 (make-obj 'crater (make-vec2 10 3) 2)))
    (remember-objects (list o1 o2 o3 o4))
    (pretty-list-of-length 1 (reachable-states (make-directed-arc (make-arc o2 o3 o1) -1)))
    ))


(define (state-center s)
  (match s
    ((struct vec2 (_ _))
     s)
    ((struct directed-arc (arc direction))
     (obj-pos (arc-host-obj arc)))
    (_ (printf "state-center: strange argument ~a~n" s)
       (assert #f))))

(define (distance state1 state2)
  (+ (vec2-distance (state-center state1) (state-center state2))
     (if (directed-arc? state2)
         (* (- pi 1) (obj-radius (arc-host-obj (directed-arc-arc state2))))
         0)))
         
              
(define (astar start)
  (define num-calls-to-reachable-states 0)
  (define (move-vector state)
    (inc! num-calls-to-reachable-states)
    (let* ((next-states (reachable-states state))
           (n (length next-states))
           (nn (* 2 n))
           (v (make-vector nn))
           (j 0))
      (while (< j nn)
             (let ((next-state (pop! next-states)))
               (vector-set! v j next-state)
               (inc! j)
               (vector-set! v j (distance state next-state))
               (inc! j)))
      v))
  (define memoized-move-vector (memoize 2000 move-vector))
  (define (generate-moves! state yield!)
    (let* ((v (memoized-move-vector state))
           (nn (vector-length v))
           (j 0))
      (while (< j nn)
             (let* ((next-state (begin0 (vector-ref v j) (inc! j)))
                    (distance   (begin0 (vector-ref v j) (inc! j))))
               (yield! distance next-state next-state)))))
  (define (lower-bound state)
     ; not really a lower bound, but this reduces cpu usage
    (distance start state))
  (define memoized-lower-bound (memoize 2000 lower-bound))

  (define (goal? state) (equal? state vec2-origin))
  (let ((result (let/ec return
                  (a* start goal? memoized-lower-bound generate-moves!
                      (lambda (solution cost) (return (list (reverse solution) cost))))
                  #f)))
    (printf "num calls to reachable-states: ~a~n" num-calls-to-reachable-states)
    result))

(define (astar-test-wall)
  ; can we get around a wall?
  (define obj1 (make-obj 'boulder (make-vec2 0 -30) 15))
  (define obj2 (make-obj 'boulder (make-vec2 10 -30) 15))
  (define obj3 (make-obj 'boulder (make-vec2 20 -30) 15))
  (clear-remembered)
  (remember-object obj1)
  (remember-object obj2)
  (remember-object obj3)
  (astar (make-vec2 5 -60)))

(define (astar-test-barely-nudging-wall)
  ; can we get around a wall of barely-touching boulders?
  (define obj1 (make-obj 'boulder (make-vec2 0 -30) 5))
  (define obj2 (make-obj 'boulder (make-vec2 10 -30) 5))
  (define obj3 (make-obj 'boulder (make-vec2 20 -30) 5))
  (clear-remembered)
  (remember-object obj1)
  (remember-object obj2)
  (remember-object obj3)
  (astar (make-vec2 5 -60)))


(define current-path #f)

(define (must-recompute-path)
  (set! current-path #f))

(define (compute-path pos)
  ; path memoization is done in compute-target instead
  ;(set! current-path (or current-path (astar pos)))
  (set! current-path (astar pos))
  (printf "astar solution: ~a~n" current-path)
  (when current-path
    (printf "astar solution length: ~a~n" (length current-path)))
  current-path)

(define (get-goal-point pos goal)
  (match goal
    ((struct vec2 (_ _)) goal)
    ((struct directed-arc (arc direction))
     (tangent pos (obj-pos (arc-host-obj arc))
              (safe-radius (arc-host-obj arc))
              (- direction)))))

(define (get-goal-ignore-list goal)
  (match goal
    ((struct directed-arc ((struct arc (home ccw cw)) direction))
     (list home))
    (else (list))))

(define (get-path-goal pos path)
  (match path
    ((list solution cost)
     (let ((last-reachable-point #f))
       (for-each (lambda (goal)
                   (let ((goal-point (get-goal-point pos goal))
                         (goal-ignore-list (get-goal-ignore-list goal)))
                     (unless (line-obstructed? pos goal-point goal-ignore-list)
                       (set! last-reachable-point goal-point))))
                 solution)
       (when (not last-reachable-point) (printf "path contains no reachable goals: ~a~n" solution))
       last-reachable-point))
    (else (printf "no solution found") #f)))

(define (compute-target pos)
  (or (and current-path (get-path-goal pos current-path))
      (get-path-goal pos (compute-path pos))
      (old-compute-target pos)))

(define (draw-path from-pos)
  (when (and current-path (gfx-on?))
    (dolist (state (car current-path))
            (let ((to-pos (state-center state)))
              (gfx-line (vec2-x from-pos) (vec2-y from-pos)
                        (vec2-x to-pos) (vec2-y to-pos))
              (set! from-pos to-pos)))))


(define (test)
  (test-arc-contains-point)
  (test-ti->da)
  (test-circle-reachability)
  (test-arc-reachability)
  (test-arc-reachability2)
  (astar-test-wall)
  (astar-test-barely-nudging-wall)
  )