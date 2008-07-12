#lang scheme

(require "messages.scm")
(require "network.scm")
(require "remember.scm")
(require "angles.scm")
(require "intersect.scm")
(require (prefix-in control- "control.scm"))
(require "path.scm")
(require "misc-syntax.ss")
(require "intersect.scm")
(require "tangent.scm")

(provide handle-message)

(define (effective-radius o)
  (match (object-kind o)
    ('boulder (+ (object-radius o) 1/2))
    ('crater  (object-radius o))))

(define (handle-message m)
  ;(printf "~a~n" m)
  (cond
    ((init? m)
     (control-init (init-max-turn m) (init-max-hard-turn m))
     (clear-remembered)
     )
    ((end? m)
     (control-clear))
    ((bump? m))
    ((success? m)
     (control-clear))
    ((failure? m)
     (control-clear))
    ((telemetry? m)
     (remember-objects (telemetry-seen m))
     ;(print-remembered)
     (let* ((t (telemetry-time m))
            (self (telemetry-vehicle m))
            (x (vehicle-x self))
            (y (vehicle-y self))
            (dir (vehicle-dir self))
            (speed (vehicle-speed self))
            (target-x 0)
            (target-y 0)
            (last-blocking-obj #f))

       (define (target-blocked?)
         ; return the object that blocks it or #f
         (let/ec return
           (hash-for-each remembered
                          (lambda (obj junk)
                            (let ((r (effective-radius obj)))
                              ; if there's an intersection that happens before
                              ; target-distance, (return obj)
                              (unless (equal? obj last-blocking-obj)
                                (when (line-intersects-circle? x y target-x target-y
                                                               (object-x obj) (object-y obj) r)
                                  (return obj))))))
           ; no object is blocking
           #f))
       
       (let avoidance-loop ()
         (printf ".")
         (let ((b (target-blocked?)))
           (when b
             ; adjust target to be the left tangent point
             ; of b
             (set! last-blocking-obj b)
             (let-values (((tx ty ta td)
                           (tangent x y
                                    (object-x b) (object-y b) (effective-radius b)
                                    1)))
               (set!-values (target-x target-y) (values tx ty)))
             (avoidance-loop))))
       (printf "~n")

       (let* (;(target-distance (sqrt (+ (sqr x) (sqr y))))
              (target-dir (atan-deg (- y) (- x)))
              (dir-target-diff (deg- target-dir dir))
              (steer (* 2 dir-target-diff))
              (accel (cond
                       ; pedal to the medal as long as we're not *completely* off course!
                       ((< (abs dir-target-diff)  120) 1)
                       ((< (abs dir-target-diff)  120) 0)
                       (else -1))))
         (control-set-state-deg/sec accel steer))))))