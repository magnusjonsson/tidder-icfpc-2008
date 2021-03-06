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
(require (prefix-in gfx- "graphics.scm"))

(provide handle-message)

(define (preprocess-object o)
  (match o
    ((struct obj ('boulder pos r))
     (make-obj 'boulder pos (+ r 1/2)))
    (_ o)))

(define max-sensor #f)

(define last-dir-target-diff 0)

;Martian avoidance parameters
(define viewing-width (/ 60 2))
(define viewing-distance 40)
(define move-away-strength-1/ 1.5)

; whether to compute something on inbox-empty
(define current-telemetry #f)

(define (handle-message m)
  ;(when (< (random) .1) (/ 0))
  ;(printf "~a~n" m)
  (cond
    ((init? m)
     (set! max-sensor (init-max-sensor m))
     (control-init (init-max-turn m) (init-max-hard-turn m))
     (clear-remembered)
     (must-recompute-path)
     (gfx-dx (init-dx m))
     (gfx-dy (init-dy m))
     )
    ((end? m)
     (set! current-telemetry #f)
     (control-clear)
     (must-recompute-path))
    ((bump? m)
     (printf "Bump!~n")
     (control-set-state -1 #f))
    ((success? m)
     (set! current-telemetry #f)
     (control-clear)
     (must-recompute-path))
    ((failure? m)
     (set! current-telemetry #f)
     (control-clear)
     (must-recompute-path))
    ((telemetry? m)
     
     (remember-objects (map preprocess-object (telemetry-seen m)))
     
     (when (remembered-dirty?)
       (clear-remembered-dirty)
       (must-recompute-path)
       )
     
     (set! current-telemetry m)
     
     (let* ((t (telemetry-time m))
            (self (telemetry-vehicle m))
            (pos (vehicle-pos self))
            (dir (vehicle-dir self))
            (speed (vehicle-speed self)))
       
       (speedometer-update t pos)
       (turnometer-update t dir)))
    
    ((inbox-empty? m)
     (when current-telemetry
       (set! m current-telemetry)
       (set! current-telemetry #f)
       
       (let* ((t (telemetry-time m))
              (self (telemetry-vehicle m))
              (pos (vehicle-pos self))
              (dir (vehicle-dir self))
              (speed (vehicle-speed self))
              (target (compute-target pos)))
         
         (printf "~n")
         (printf "target: ~a ~n" target)
         
         (let* ((relative (curryr vec2- pos))
                (target-rel (relative target))
                (target-dir (vec2-angle-deg target-rel))
                (martians-rel
                 (map (compose relative vehicle-pos)
                      (filter vehicle? (telemetry-seen m))))
                (ahead? (lambda (x) (< (abs (deg- target-dir x)) viewing-width)))
                (martians-close
                 (filter (compose (curryr < viewing-distance) vec2-length) martians-rel))
                (martians-ahead
                 (filter (compose ahead? vec2-angle-deg) martians-close))
                (martian-mean-dir
                 (and (not (null? martians-ahead))
                      (vec2-angle-deg
                       (apply vec2+ (map vec2-normalize martians-ahead)))))
                (adjusted-dir
                 (if martian-mean-dir
                     (vec2-angle-deg
                      (vec2+ (vec2-scale (sub1 move-away-strength-1/)
                                         (vec2-normalize target-rel))
                             (angle-deg->vec2
                              (- target-dir (- (- target-dir martian-mean-dir))))))
                     target-dir))
                
                ; search forwards to see at what point we will crash going forwards, if any.
                (crash-distance (first-hit-time pos (angle-deg->vec2 dir)))
                (dir-target-diff (deg- adjusted-dir dir))
                (steer (* 2 dir-target-diff))
                (wanted-speed (min
                               ; drive slower when trying to turn
                               (/ 320.0 (max 0.001 (abs steer)))
                               ; drive slower when rotating
                               (/ 320.0 (max 0.001 (abs (turnometer-value))))
                               ; try to be able to stop in time if something shows up ahead.
                               (* 2.0 (sqrt (max 0 (- (or crash-distance +inf.0) safety-margin))))
                               (* 2.0 (sqrt (max 0 (- max-sensor safety-margin))))
                               ))
                (speed (speedometer-value))
                
                (accel (sgn (- wanted-speed speed))))
           
           ;(printf "crash-distance: ~a~n" crash-distance)
           (printf "accel: ~a steer: ~a  speed: ~a wanted-speed: ~a~n" accel steer speed wanted-speed)
           ;(unless (> .001 (abs (- target-dir adjusted-dir)))
           ;  (printf "Original direction: ~a~nAdjusted:~a~n" target-dir adjusted-dir))
           ;(printf "target-distance: ~a~n" target-distance)
           (control-set-state-deg/sec accel steer)
           
           (when (gfx-on?)
             ; background
             (gfx-color 255 255 255)
             (gfx-clear)
             ; draw remembered
             (gfx-color 255 0 0)
             (draw-remembered)
             ; draw home base
             (gfx-color 0 255 0)
             (gfx-circle 0 0 5)
             ; draw A* path
             (gfx-color 128 128 128)
             (draw-path pos)
             ; draw line to target
             (gfx-color 128 128 255)
             (gfx-line (vec2-x pos) (vec2-y pos) (vec2-x target) (vec2-y target))
             ; draw rover
             (gfx-color 0 0 255)
             (gfx-circle (vec2-x pos) (vec2-y pos) 5)
             ; show it
             (gfx-show)
             )))))))
