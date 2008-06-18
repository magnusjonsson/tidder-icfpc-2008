#lang scheme

(require srfi/43) ; vector-swap!

(provide make
         prio?
         empty?
         length
         insert!
         extract-min!
         )

(define-struct prio
  (<-fn 
   count
   vector
   )
  #:mutable)

(define min-vector-length 16)

(define (make <-fn)
  (make-prio <-fn 0 (make-vector min-vector-length)
             ))

(define (allocated p)
  (vector-length (prio-vector p)))

(define (full? p)
  (= (prio-count p)
     (allocated p)))

(define (grow! p)
  (let* ((old-vector (prio-vector p))
         (old-length (vector-length old-vector))
         (new-length (max min-vector-length (* 2 old-length)))
         (new-vector (make-vector new-length)))
    (vector-copy! new-vector 0 old-vector)
    (set-prio-vector! p new-vector)))

(define (last-index p)
  (- (prio-count p) 1))

(define (parent-index index)
  (quotient (- index 1) 2))

(define (first-child-index index)
  (+ (* index 2) 1))

(define (second-child-index index)
  (+ (* index 2) 2))

(define (prio-ref p index)
  (vector-ref (prio-vector p) index))

(define (prio-set! p index value)
  (vector-set! (prio-vector p) index value))

(define (inc-count! p)
  (set-prio-count! p (+ 1 (prio-count p))))

(define (push! p item)
  (when (full? p)
    (grow! p))
  (prio-set! p (prio-count p) item)
  (inc-count! p))

(define (bubble-up! p i)
  (let ((j (parent-index i))
        (v (prio-vector p)))
    (when ((prio-<-fn p) (vector-ref v i)
                         (vector-ref v j))
      (vector-swap! v i j)
      (bubble-up! p j))))

(define (insert! p item)
  (push! p item)
  (bubble-up! p (last-index p)))

(define (empty? p)
  (= (prio-count p) 0))

(define length prio-count)

(define (bubble-down! <-fn v i vi c)
  (let* ((j (first-child-index i))
         (k (+ j 1)))
    (if (>= j c)
        (vector-set! v i vi)
        (let* ((vj (vector-ref v j))
               (vk (if (>= k c) vj (vector-ref v k))))
          (if (<-fn vk vj)
              (bubble-down-aux! <-fn v i vi k vk c)
              (bubble-down-aux! <-fn v i vi j vj c))))))

(define (bubble-down-aux! <-fn v i vi j vj c)
  (if (<-fn vj vi)
      (begin
        (vector-set! v i vj)
        (bubble-down! <-fn v j vi c))
      (vector-set! v i vi)))

(define (extract-min! p)
  (let* ((v            (prio-vector p))
         (first-value  (vector-ref v 0))
         (last-index   (- (prio-count p) 1))
         (last-value   (vector-ref v last-index)))
    (set-prio-count! p last-index)
    (vector-set! v last-index #f)
    (bubble-down! (prio-<-fn p) v 0 last-value last-index)
    first-value))

(define (test)
  (define a (make <))
  (insert! a 1)
  (printf "~s~n" (extract-min! a))
  (printf "--------~n")
  (insert! a 1)
  (insert! a 3)
  (insert! a 2)
  (insert! a 5)
  (insert! a 0)
  (insert! a 4)
  (printf "~s~n" (extract-min! a))
  (printf "~s~n" (extract-min! a))
  (printf "~s~n" (extract-min! a))
  (insert! a 7)
  (insert! a 6)
  (printf "~s~n" (extract-min! a))
  (printf "~s~n" (extract-min! a))
  (printf "~s~n" (extract-min! a))
  (printf "~s~n" (extract-min! a))
  (printf "~s~n" (empty? a))
  )