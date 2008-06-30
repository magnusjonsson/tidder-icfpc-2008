#lang scheme

(provide genetic-optimize)

;General advise:
;Use a big starting population so you don't get stuck in local minima
;Use less normal population members for speed
;Memoize evaluate by (size * (mixfactor + 1))
;Let mix mutate the values a bit
;Higher mixfactor gives better results for slightly slower computation
(define (genetic-optimize pop size evaluate mix mixfactor callback)
  (let loop ((pop pop))
    (let ((winners (take-sort (lambda (x y) (<=> (evaluate y) (evaluate x)))
                              pop size)))
      (callback winners)
      (loop 
       (append (vector->list winners)
               (for/list ((c (in-range (* mixfactor size))))
                 (mix (vector-ref winners (random size))
                      (vector-ref winners (random size)))))))))

;Feels like it should be built in somewhere...
(define (<=> x y)
  (cond ((> x y) 1)
        ((= x y) 0)
        (else   -1)))

;Take the n lowest (as per <=>) items of l
;Sorts no more than is needed
(define (take-sort <=> l n)
  (let/ec k
    (define i 0)
    (define a (make-vector n))
    (define (add x)
      (vector-set! a i x)
      (set! i (add1 i))
      (when (= i n) (k a)))
    
    (let quicksort ((l l))
      (unless (null? l)
        (let ((pivot (car l)) (rest (cdr l)))
          (call-with-values
           (lambda ()
             (for/fold ((left '()) (center (list pivot)) (right '())) ((x rest))
               (case (<=> x pivot)
                 ((-1) (values (cons x left) center right))
                 (( 0) (values left (cons x center) right))
                 (( 1) (values left center (cons x right))))))
           (lambda (left center right)
             (quicksort left)
             (for-each add center)
             (quicksort right))))))))
