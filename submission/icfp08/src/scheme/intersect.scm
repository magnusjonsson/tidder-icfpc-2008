#lang scheme

(provide line-intersects-circle?)

(define (line-intersects-circle? line-x0 line-y0 line-x1 line-y1 circle-x circle-y r)
  (when (> line-x0 line-x1)
    (set!-values (line-x0 line-x1) (values line-x1 line-x0))
    (set!-values (line-y0 line-y1) (values line-y1 line-y0)))
  
  (and (>= (+ circle-x r) line-x0)
       (<= (- circle-x r) line-x1)

       (let* ((a (/ (- line-y0 line-y1) (- line-x0 line-x1)))
              (b (- line-y0 (* line-x0 a)))
              (c (sqrt (+ 1 (sqr a))))
              (D (- (sqr r) (sqr (/ (- circle-y (* a circle-x) b) c)))))
         (and (not (negative? D))
              (let ((rh (/ (sqrt D) c))
                    (lh (/ (+ (* a circle-y) circle-x (* a b -1))
                           (+ 1 (sqr a)))))
                (or (<= line-x0 (- lh rh) line-x1)
                    (<= line-x0 (+ lh rh) line-x1)))))))

; returns the first intersection or (values #f #f)
(define (ray-circle origin-x origin-y ray-dx ray-dy
                    circle-x circle-y circle-r)
    ; |origin + t * ray - circle |^2 - r^2 = 0
  (let ((dx (- origin-x circle-x))
        (dy (- origin-y circle-y))
        (r2 (sqr circle-r)))
    ; |t * ray + diff |^2 - r^2 = 0
    ; (dx + t * rx)^2 + (..y..) - r^2 = 0
    ; dx^2 + 2*t*dx*rx + t^2*rx^2 + (..y..) - r^2 = 0
    ; (dx^2+dy^2-r^2) + (2*(dx*rx+dy*ry))*t + (rx^2+ry^2)*t^2 = 0
    ; C                     + B*t         + A*t^2           = 0
    (let ((c (- (+ (sqr dx) (sqr dy))
                r2))
          (b (* 2 (+ (* dx ray-dx) (* dy ray-dy))))
          (a (+ (sqr ray-dx) (sqr ray-dy))))
      (printf "~a ~a ~a~n" a b c)
      (let loop ((ts (solve-quadratic a b c)))
        (if (empty? ts)
            (values #f #f)
            (let ((t (car ts)))
              (if (>= t 0)
                  (values (+ origin-x (* t ray-dx))
                          (+ origin-y (* t ray-dy)))
                  (loop (cdr ts)))))))))

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
