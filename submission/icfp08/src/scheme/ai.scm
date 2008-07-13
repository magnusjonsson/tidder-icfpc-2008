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
(require "vec2.scm")
(require (prefix-in speedometer- "speedometer.scm"))

(provide handle-message)

(define safety-margin 1.0) ; 100% of vehicle's width

(define (safe-radius o)
  (+ safety-margin
     (obj-radius o)))

(define (preprocess-object o)
  (match o
    ((struct obj ('boulder pos r))
     (make-obj 'boulder pos (+ r 1/2)))
    (_ o)))

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
     (remember-objects (map preprocess-object (telemetry-seen m)))
     (printf "remembered objects: ~a~n" (hash-count remembered))
     (let* ((t (telemetry-time m))
            (self (telemetry-vehicle m))
            (pos (vehicle-pos self))
            (dir (vehicle-dir self))
            (speed (vehicle-speed self))
            (target (make-vec2 0 0))
            (last-blocking-obj #f))

       (define (target-blocked?)
         ; return the object that blocks it or #f
         (let/ec return
           (hash-for-each remembered
                          (lambda (obj junk)
                            ; if there's an intersection that happens before
                            ; target-distance, (return obj)
                            (unless (equal? obj last-blocking-obj)
                              (when (line-intersects-circle?-alt pos target
                                                                 (obj-pos obj)
                                                                 (safe-radius obj))
                                (return obj)))))
           ; no object is blocking
           #f))
       
       (let avoidance-loop ()
         (printf ".")
         (let ((b (target-blocked?)))
           (when b
             ; adjust target to be the left tangent point
             ; of b
             (set! last-blocking-obj b)
             (set! target (tangent pos (obj-pos b) (safe-radius b) 1))
             (avoidance-loop))))
       (printf "~n")
       (printf "target: ~a ~n" target)
       (speedometer-update t pos)

       (let* ((target-dir (vec2-angle-deg (vec2- target pos)))
              (target-distance (vec2-distance target pos))
              (dir-target-diff (deg- target-dir dir))
              (steer (* 3 dir-target-diff))
              ; don't accelerate so much if you are
              ; turning and you are very close to the target
              (wanted-speed (* 15 ; braveness factor
                              (/ (max 1 (abs dir-target-diff)))
                              target-distance
                              ))
              (speed (speedometer-value))
                            
              (accel (cond
                       ((> wanted-speed speed) 1)
                       ((> wanted-speed speed) 0)
                       (else -1))))
         (printf "accel: ~a steer: ~a  speed: ~a wanted-speed: ~a~n" accel steer speed wanted-speed)
         (printf "target-distance: ~a~n" target-distance)
         (control-set-state-deg/sec accel steer))))))