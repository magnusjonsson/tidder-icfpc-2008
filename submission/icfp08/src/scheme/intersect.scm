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
                    (<= line-x1 (+ lh rh) line-x1)))))))
