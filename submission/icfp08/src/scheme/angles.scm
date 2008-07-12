#lang scheme

(provide rad->deg deg- normalize-deg unwrap-deg atan-deg)

(define (rad->deg rad)
  (* 180 (/ rad pi)))

(define (deg- d1 d2)
  (let ((diff (- d1 d2)))
    (normalize-deg diff)))

(define (normalize-deg deg)
  (- deg (* 360 (round (/ deg 360)))))

(define (unwrap-deg reference deg)
  (+ reference (deg- deg reference)))

(define (atan-deg x y) (rad->deg (atan (- 0 y) (- 0 x))))
