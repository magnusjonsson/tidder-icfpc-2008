#lang scheme

(provide init set-state-deg/sec clear)

(require "network.scm")
(require "misc-syntax.ss")

(define steer-state 0)
(define accel-state 0)

(define (clear)
  (set! steer-state 0)
  (set! accel-state 0))

(define mt 0) ; max turn
(define mth 0) ; max hard turn

(define (init max-turn max-turn-hard)
  (set! mt max-turn)
  (set! mth max-turn-hard))

(define (set-state as ss)
  (printf "~a ~a~n" as ss)
  (let ((accel-adjustment "")
        (steer-adjustment ""))
    (when (not (= as accel-state))
      (set! accel-adjustment (match as (-1 "b") (0 "") (1 "a")))
      (set! accel-state as))
    (cond
      ((< ss steer-state) (set! steer-adjustment "r") (dec! steer-state))
      ((> ss steer-state) (set! steer-adjustment "l") (inc! steer-state)))
    (when (or (not (equal? "" steer-adjustment))
              (not (equal? "" accel-adjustment)))
      (let ((s (format "~a~a;" accel-adjustment steer-adjustment)))
        (send-string s)
        (printf "sending: ~a~n" s))
      (set-state as ss))))

(define (set-state-deg/sec as deg/sec)
  (printf "~a ~a~n" as deg/sec)
  (set-state as (deg/sec->steer-state deg/sec)))

(define (deg/sec->steer-state rate)
  (cond
    ((< rate (- mth)) -2)
    ((< rate (- mt)) (closest rate (- mth) (- mt) -2 -1))
    ((< rate 0)    (closest rate (- mt) 0 -1 0))
    ((<= rate mt)   (closest rate 0 mt 0 1))
    ((<= rate mth)  (closest rate mt mth 1 2))
    (else             2)))

(define (closest x x0 x1 r0 r1)
  (if (< (* (- x1 x0) (random))
         (- x x0))
      r1
      r0))