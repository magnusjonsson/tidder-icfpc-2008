#lang scheme

(provide fit acceleration)

(require srfi/43) ; vector-swap!
(require (only-in rnrs/base-6 assert))
(require "misc-syntax.ss")

(define (matrix-ref m row col)
  (vector-ref (vector-ref m row) col))

(define (matrix-set! m row col value)
  (vector-set! (vector-ref m row) col value))

(define (row-reduce! m)
  ; turn the left side of a matrix into
  ; a diagonal matrix by performing only row operations

  (define (swap-rows row1 row2)
    (vector-swap! m row1 row2))

  (define (scale-sub dst-row src-row factor)
    (let* ((dst (vector-ref m dst-row))
           (src (vector-ref m src-row))
           (cols (min (vector-length src) (vector-length dst))))
      (dotimes (col cols)
               (vector-set! dst col (- (vector-ref dst col)
                                       (* factor (vector-ref src col)))))))
  
  
  (define rows (vector-length m))
  
  (dotimes (col rows)
           (let ((pivot-row col))
             (dotimes (row rows)
                      (when (and (> row col)
                                 (> (abs (matrix-ref m row col))
                                    (abs (matrix-ref m pivot-row col))))
                        (set! pivot-row row)))
             
             (swap-rows pivot-row col)
             
             (dotimes (row rows)
                      (unless (= row col)
                        (scale-sub row col (/ (matrix-ref m row col)
                                              (matrix-ref m col col)))
                        (matrix-set! m row col 0))))))


(define (fit t0 v0 t1 v1 t2 v2)
; Fit a quadratic of the form
; v(t) = a0 + a1*t + a2*t^2
; to the three points (t0,v0) (t1,v1) (t2,v2).
;
; To do so, we solve this equation:
;
; | 1 t0 t0^2 |   | x0 |   | v0 |
; | 1 t1 t1^2 | * | x1 | = | v1 |
; | 1 t2 t2^2 |   | x2 |   | v2 |
;       A       *    x   =   B
;
;
;
;      A^-1 * A   *    x   =   A^-1 * B
;
;
;
  
  ; form the augmented matrix A|B
  (let ((m (vector (vector 1 t0 (* t0 t0) v0)
                   (vector 1 t1 (* t1 t1) v1)
                   (vector 1 t2 (* t2 t2) v2))))
    ; row reduce it
    (row-reduce! m)
    
    ; normalize and return the B part of the augmented matrix
    (vector (/ (matrix-ref m 0 3) (matrix-ref m 0 0))
            (/ (matrix-ref m 1 3) (matrix-ref m 1 1))
            (/ (matrix-ref m 2 3) (matrix-ref m 2 2)))))
  
(define (acceleration q)
  ; differentiate twice to get acceleration
  (* 2 (vector-ref q 2)))


(define (test)
  (assert (equal? (fit 0 0  1 0  2 0)
                  (vector 0 0 0)))
  (assert (equal? (fit 0 0  1 1  2 4)
                  (vector 0 0 1)))
  (assert (equal? (fit 0 2  1 1  2 0)
                  (vector 2 -1 0)))
  (assert (equal? (acceleration (fit 0 0    1 1    2 0))
                  (acceleration (fit 0 179  1 180  2 179))))
  (assert (equal? (acceleration (fit 0 -8    1 -2    2 4))
                  (acceleration (fit 0 172  1 178  2 184))))
  )