#lang scheme

(provide init quit on? dx dy color clear show line circle)

(require scheme/system)

(define out #f)
(define in #f)
(define err #f)

(define (init gfx-program)
  (match (process gfx-program)
    ((list o i pid e control)
     (set! out o)
     (set! in i)
     (set! err e))
    (_ (printf "graphics init error~n"))))

(define (quit)
  (send "quit~n")
  (close-input-port out)
  (close-output-port in)
  (close-input-port err)
  (set! out #f)
  (set! in #f)
  (set! err #f)  
  )
(define (dx value) (send "dx ~a~n" value))
(define (dy value) (send "dy ~a~n" value))

(define (on?)
  (if in #t #f))

(define (send format . args)
  (when in
    (apply fprintf in format args)
    (flush-output in)))

(define (color r g b) (send "color ~a ~a ~a~n" r g b))
(define (clear) (send "clear~n"))
(define (show) (send "show~n"))
(define (line x1 y1 x2 y2) (send "line ~a ~a ~a ~a~n" x1 y1 x2 y2))
(define (circle x y r) (send "circle ~a ~a ~a~n" x y r))

(define (test)
  (init)
  (dx 100)
  (dy 100)
  (color 255 255 255)
  (circle 0 0 50)
  (show))