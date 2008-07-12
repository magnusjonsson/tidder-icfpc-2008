#lang scheme

; this module estimates how how fast a vehicle can steer based on observed (time,direction) pairs.
;
; it ignores the effect of reaching the target turning rate.

(provide init learn
         max-acceleration
         curr-quadratic
         curr-acceleration
         curr-speed
         curr-position
         clear-history)

(require (prefix-in quadratic- "quadratic.scm"))
(require "misc-syntax.ss")
(require "angles.scm")
(require (only-in rnrs/base-6 assert))

(define q (vector 0 0 0)) ; current quadratic
(define cp 0) ; estimated current position
(define cs 0) ; estimated current speed
(define ca 0) ; estimated current acceleration
(define ma 0) ; estimated max turning acceleration (actually, the fastest turning acceleration seen so far)

(define mt 0)
(define mth 0)

(define (init max-turn max-turn-hard)
  (set! mt max-turn)
  (set! mth max-turn-hard))

; last three observed (time,direction) pairs
(define-values (t0 t1 t2) (values #f #f #f))
(define-values (d0 d1 d2) (values #f #f #f))

(define (clear-history)
  (set! t0 #f)
  (set! d0 #f)
  (set! t1 #f)
  (set! d1 #f)
  (set! t2 #f)
  (set! d2 #f))

(define (record time dir)
  (if (equal? time t2)
      (set! d2 dir)
      (begin
        ; shift in new values
        (set! t0 t1)
        (set! d0 d1)
        (set! t1 t2)
        (set! d1 d2)
        (set! t2 time)
        (set! d2 dir))))

(define (have-three-points?)
  (and t0 t1 t2 d0 d1 d2))

(define (learn time dir)
  (record time dir)
  ; fit a quadratic through the last three observed (time,dir) pairs,
  ; measure its acceleration, and if it is faster than anything before
  ; seen, conclude that  we must be able to steer at least that fast.
  
  ; since angles can wrap around we use the "most plausible explanation"
  ; for which direction the turning went
  (when (have-three-points?)
    (set! q (quadratic-fit t0 (unwrap-deg d1 d0)
                           t1 (unwrap-deg d1 d1)
                           t2 (unwrap-deg d1 d2)))
    (set! cp (quadratic-value-at q t2))
    (set! cs (quadratic-speed-at q t2))
    (set! ca (quadratic-acceleration q))
    (max! ma (abs ca))))

(define (reset)
  (clear-history)
  (set! ma 0))

(define (max-acceleration)
  ma)

(define (curr-quadratic)
  q)

(define (curr-acceleration)
  ca)

(define (curr-speed)
  cs)

(define (curr-position)
  cp)

(define (test)
  (define (should= a b c
                   d e f)
    (let ((e1 (begin 
                (reset)
                (learn 0 a) (learn 1 b) (learn 2 c)
                (max-acceleration)))
          (e2 (begin
                (reset)
                (learn 0 d) (learn 1 e) (learn 2 f)
                (max-acceleration))))
      (unless (= e1 e2)
        (printf "should be equal: ~a ~a~n" e1 e2)
        (printf "~a ~a ~a ~a ~a ~a~n" a b c d e f))))
  (should= 172 178 -176
           -8 -2 4)
  )