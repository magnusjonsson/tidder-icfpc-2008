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

(provide handle-message)

(define safety-margin 2.5)

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

       (let* ((target-dir (vec2-angle-deg (vec2- target pos)))
              (target-distance (vec2-distance target pos))
              (dir-target-diff (deg- target-dir dir))
              (steer (* 2 dir-target-diff))
              ; don't accelerate towards the side if you are
              ; close to the target
              (accel-angle (min 120 (* 10 target-distance)))
              
              (accel (cond
                       ; pedal to the medal as long as we're not *completely* off course!
                       ((< (abs dir-target-diff) accel-angle) 1)
                       ((< (abs dir-target-diff)  120) 0)
                       (else -1))))
         (control-set-state-deg/sec accel steer))))))