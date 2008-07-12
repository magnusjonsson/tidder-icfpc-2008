#lang scheme

(require "messages.scm")
(require "network.scm")
(require "remember.scm")
(require "angles.scm")
(require (prefix-in sse- "steering-speed-estimator.scm"))

(provide handle-message)

(define (send-steer-accel s a)
  (let ((str (format "~a~a;"
                       (match a (-1 "b") (0 "") (1 "a"))
                       (match s (-1 "r") (0 "") (1 "l")))))
    (printf "~a~n" str)
    (send-string str)))


(define (handle-message m)
  (printf "~a~n" m)
  (cond
    ((init? m)
     (sse-clear-history))
    ((end? m)
     (sse-clear-history))
    ((bump? m)
     (sse-clear-history))
    ((success? m)
     (sse-clear-history))
    ((failure? m)
     (sse-clear-history))
    ((telemetry? m)
     (remember-objects (telemetry-seen m))
;     (print-remembered)
     (let* ((t (telemetry-time m))
            (self (telemetry-vehicle m))
            (x (vehicle-x self))
            (y (vehicle-y self))
            (dir (vehicle-dir self))
            (speed (vehicle-speed self))
            (target-dir (rad->deg (atan (- 0 y) (- 0 x)))))
       (sse-learn t dir)
       (let* ((dir-max-accel (sse-max-acceleration))
              (dir-target-diff (deg- target-dir dir))
              (steer (if (< 0 dir-target-diff)
                         1
                         -1))
              (accel (cond
                       ; not sure if this is the best way to compensate for steering speed...
                       ((< (* dir-max-accel (abs dir-target-diff))  50000) 1)
                       ((< (* dir-max-accel (abs dir-target-diff)) 100000) 0)
                       (else -1))))
         (printf "sse: ~a~n" dir-max-accel)
         (send-steer-accel steer accel))))))