#lang scheme

(require (prefix-in msg: "messages.scm"))
(require "libraries/cache/memoize.scm")

(provide remember-objects remember-object
         print-remembered init-remembered
         collides)

(define raster-size 10)

(define dx #f)
(define dy #f)
(define x-num #f)
(define y-num #f)

(define grid #f)

(define (remember-objects objects)
  (for-each remember-object objects))

(define (remember-object o)
  ;Only objects allowed so far
  (when (msg:object? o)
    ;Get grid position for x y
    (let ((r (get-raster (msg:object-x o) (msg:object-y o))))
      (and r
           (let ((x-i (car r))
                 (y-i (cadr r)))
             ;If it's not a hash yet, make it
             (unless (grid-ref x-i y-i)
               (grid-set! x-i y-i (make-hash)))
             ;Insert into hash
             (hash-set! (grid-ref x-i y-i) o #t))))))

(define (print-remembered)
  (printf "remembered objects:~n")
  (for* ((x (in-range x-num))
         (y (in-range y-num)))
    (let ((cur (grid-ref x y)))
      (when cur
        (printf "~a ~a:" x y)
        (hash-for-each cur (lambda (key value) (printf " ~a" key)))
        (printf "~n")))))

(define (init-remembered my-dx my-dy)
  (set! dx my-dx)
  (set! dy my-dy)
  (set! x-num (quotient (+ dx raster-size -1) raster-size))
  (set! y-num (quotient (+ dy raster-size -1) raster-size))
  (set! grid (make-vector (* x-num y-num) #f)))

(define (get-raster x y)
  (let ((x (+ x (/ dx 2))) (y (+ y (/ dy 2))))
    (and (< x dx) (>= x 0) (< y dy) (>= y 0)
         (list (quotient (inexact->exact (round x)) raster-size)
               (quotient (inexact->exact (round y)) raster-size)))))

(define (grid-ref x y)
  (vector-ref grid (+ x (* x-num y))))

(define (grid-set! x y value)
  (vector-set! grid (+ x (* x-num y)) value))

(define (collides? x y)
  (define buffer-zone 0.1) ;slightly overestimate so we don't have to be pixel-perfect
  
  (define (collision1? break)
    (lambda (o _)
      (let ((distance
             (sqrt
              (+
               (sqr (abs (- x (msg:object-x o))))
               (sqr (abs (- y (msg:object-y o))))))))
        (when (< distance
                 (case (msg:object-kind o)
                   ((builder) (+ buffer-zone .5 (msg:object-radius o)))
                   ((crater)  (+ buffer-zone (msg:object-radius o)))
                   (else 0))) ;last one should never happen
          (break #t)))))
  
  
  (define collision-grid?
    (memoize 8
             (lambda (x-i y-i)
               (let/ec break
                 (let ((h (grid-ref x-i y-i)))
                   (and h
                        (hash-for-each h (collision1? break))))
                 #f))))
  
  (define (collision? new-x new-y)
    (let ((r (get-raster new-x new-y)))
      (and r
           (let ((x-i (car r))
                 (y-i (cadr r)))
             (collision-grid? x-i y-i)))))
  
  (define check-radius 1)
  (define cr check-radius)
  
  (let ((sq (sqrt (/ cr 2))))
    (or (collision? x (+ y cr))
        (collision? x (- y cr))
        (collision? (+ x cr) y)
        (collision? (- x cr) y)
        (collision? (+ x sq) (+ y sq))
        (collision? (+ x sq) (- y sq))
        (collision? (- x sq) (+ y sq))
        (collision? (- x sq) (- y sq)))))
