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

(define remembered (make-hash))

(define (remember-objects objects)
  (for-each remember-object objects))

(define (remember-object o)
  (cond
    ((object? o) (hash-set! remembered o #t))))


(define (print-remembered)
  (printf "remembered objects:")
  (hash-for-each remembered
                 (lambda (key value)
                   (printf " ~a" key)))
  (printf "~n"))

(define (reset-remembered)
  (set! remembered (make-hash)))

(define (reset-state)
  (reset-remembered))

(define (handle-message m)
  (cond
    ((end? m)
     (reset-state))
    ((telemetry? m)
     (remember-objects (telemetry-seen m))
     (print-remembered)
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