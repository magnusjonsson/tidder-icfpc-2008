#lang scheme

(require "messages.scm")
(require "network.scm")
(provide handle-message)

(define (rad->deg rad)
  (* 180 (/ rad pi)))

(define (deg- d1 d2)
  (let ((diff (- d1 d2)))
    (normalize-deg diff)))

(define (normalize-deg deg)
  (- deg (* 360 (round (/ deg 360)))))

(define (handle-message m)
  (cond
    ((telemetry? m)
     (let* ((self (telemetry-vehicle m))
            (x (vehicle-x self))
            (y (vehicle-y self))
            (dir (vehicle-dir self))
            (speed (vehicle-speed self))
            (target-dir (rad->deg (atan (- 0 y) (- 0 x))))
            (dir-diff (deg- dir target-dir)))
       (send-string (if (< 0 dir-diff)
                        "ar;"
                        "al;"))))))