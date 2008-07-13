#lang scheme

(provide rad->deg deg->rad deg- normalize-deg unwrap-deg atan-deg)

(define (rad->deg rad)
  (* 180 (/ rad pi)))

(define (deg->rad deg)
  (* pi (/ deg 180)))

(define (deg- d1 d2)
  (let ((diff (- d1 d2)))
    (normalize-deg diff)))

(define (normalize-deg deg)
  (- deg (* 360 (round (/ deg 360)))))

(define (unwrap-deg reference deg)
  (+ reference (deg- deg reference)))

(define (atan-deg y x) (rad->deg (atan y x)))
