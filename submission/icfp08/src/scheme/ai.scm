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
(require (prefix-in turnometer- "turnometer.scm"))

(provide handle-message)

(define (preprocess-object o)
  (match o
    ((struct obj ('boulder pos r))
     (make-obj 'boulder pos (+ r 1/2)))
    (_ o)))

(define max-sensor #f)

(define last-dir-target-diff 0)

(define (handle-message m)
  ;(printf "~a~n" m)
  (cond
    ((init? m)
     (set! max-sensor (init-max-sensor m))
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
     (let* ((t (telemetry-time m))
            (self (telemetry-vehicle m))
            (pos (vehicle-pos self))
            (dir (vehicle-dir self))
            (speed (vehicle-speed self))
            (target (compute-target pos)))
       
       (printf "~n")
       (printf "target: ~a ~n" target)
       (speedometer-update t pos)
       (turnometer-update t dir)

       (let* ((target-dir (vec2-angle-deg (vec2- target pos)))
              ; search forwards to see at what point we will crash going forwards, if any.
              (crash-distance (first-hit-time pos (angle-deg->vec2 dir)))
              (dir-target-diff (deg- target-dir dir))
              (steer (* 2 dir-target-diff))
              (wanted-speed (min
                               ; drive slower when trying to turn
                               (/ 320.0 (max 0.001 (abs steer)))
                               ; drive slower when rotating
                               (/ 320.0 (max 0.001 (abs (turnometer-value))))
                               ; try to be able to stop in time if something shows up ahead.
                               (* 2.0 (sqrt (max 0 (- (min (or crash-distance +inf.0)
                                                           max-sensor)
                                                      safety-margin))))
                               ))
              (speed (speedometer-value))
                            
              (accel (cond
                       ((> wanted-speed speed) 1)
                       ((> wanted-speed speed) 0)
                       (else -1))))
         (printf "crash-distance: ~a~n" crash-distance)
         (printf "accel: ~a steer: ~a  speed: ~a wanted-speed: ~a~n" accel steer speed wanted-speed)
;         (printf "target-distance: ~a~n" target-distance)
         (control-set-state-deg/sec accel steer))))))