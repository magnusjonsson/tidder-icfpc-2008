#lang scheme

(provide line-intersects-circle? line-intersects-circle?-alt)
(require "vec2.scm")
(require "misc-syntax.ss")
(require (only-in rnrs/base-6 assert))

(define (line-intersects-circle? p0 p1 center radius)
  (line-intersects-circle?-raw
   (vec2-x p0) (vec2-y p0)
   (vec2-x p1) (vec2-y p1)
   (vec2-x center) (vec2-y center)
   radius))   

(define (line-intersects-circle?-raw line-x0 line-y0 line-x1 line-y1 circle-x circle-y r)
  (when (> line-x0 line-x1)
    (swap! line-x0 line-x1)
    (swap! line-y0 line-y1))
  
  (and (not (or (> line-x0 (+ circle-x r))  ; completely to the right of vertical segment
                (< line-x1 (- circle-x r))  ; completely to the left of vertical segment
                ))

       (let* ((a (/ (- line-y0 line-y1) (- line-x0 line-x1)))
              (b (- line-y0 (* line-x0 a)))
              (c (sqrt (+ 1 (sqr a))))
              (D (- (sqr r) (sqr (/ (- circle-y (* a circle-x) b) c)))))
         (and (not (negative? D))
              (let ((rh (/ (sqrt D) c))
                    (lh (/ (+ (* a circle-y) circle-x (* a b -1))
                           (+ 1 (sqr a)))))
                (or (<= line-x0 (- lh rh) line-x1)
                    (<= line-x1 (+ lh rh) line-x1)))))))

(define (line-intersects-circle?-alt p0 p1 center radius)
  (ormap (lambda (time) (<= 0 time 1))
         (ray-circle-intersection-times p0 (vec2- p1 p0) center radius)))

(define (ray-circle-quadratic origin ray center radius)
  ; |origin + t * ray - center |^2 - r^2 = 0
  (let ((diff (vec2- origin center))
        (r2 (sqr radius)))
    ; |t * ray + diff |^2 - r^2 = 0
    ; (dx + t * rx)^2 + (..y..) - r^2 = 0
    ; dx^2 + 2*t*dx*rx + t^2*rx^2 + (..y..) - r^2 = 0
    ; (dx^2+dy^2-r^2) + (2*(dx*rx+dy*ry))*t + (rx^2+ry^2)*t^2 = 0
    ; C                     + B*t         + A*t^2           = 0
    (let ((c (- (vec2-length-squared diff) r2))
          (b (* 2 (vec2-dot-product ray diff)))
          (a (vec2-length-squared ray)))
      (values a b c))))

(define (ray-circle-intersection-times origin ray center radius)
  (let-values (((a b c) (ray-circle-quadratic origin ray center radius)))
    (solve-quadratic a b c)))

; returns the first intersection point or #f
(define (ray-circle-first-intersection-point origin ray center radius)
  (ormap (lambda (time)
           (if (>= time 0)
               (vec2+ origin (vec2-scale time ray))
               #f))
         (ray-circle-intersection-times ray center radius)))

(define (solve-quadratic a b c)
  (let* ((b (* -1/2 b))
         (delta (- (* b b)
                   (* a c))))
    (cond
      ((< delta 0)
       '()) ; complex roots
      ((= delta 0)
       (list (/ b a))) ; one double root
      (else ; two distinct roots
       (let ((root (sqrt delta)))
         (list (/ (- b root) a)
               (/ (+ b root) a)))))))


(define (test)
  (define (should-intersect p0 p1 c r)
    (assert (line-intersects-circle? p0 p1 c r))
    (assert (line-intersects-circle?-alt p0 p1 c r)))
  (should-intersect (make-vec2 -10 0) (make-vec2 10 0) (make-vec2 0 0) 5)
  (should-intersect (make-vec2 0 -10) (make-vec2 0 10) (make-vec2 0 0) 5)
  )
