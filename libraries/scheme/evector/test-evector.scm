(require "srfi-check.scm"
         "evector.scm"
         (lib "42.ss" "srfi"))

(define default-fill-value (evector-ref (make-evector 1) 0))
(define DFV default-fill-value)

; evector 
; evector?
(check (evector? (vector 1 2 3)) => #f)
(check (evector? (evector 1 2 3)) => #t)
(check (evector? (evector)) => #t)

; evector-fill
(check (evector-fill (make-evector 10 'foo)) => 'foo)
(check (let ([v (make-evector 10 'foo)])
         (set-evector-fill! v 'bar)
         (evector-fill v))
       => 'bar)

; evector-fill!
(check (every?-ec (: l 100)
                  (let ([v (make-evector l)])
                    (evector-fill! v 'foo)
                    (equal? (evector->vector v)
                            (vector-ec (: i l) 'foo))))
       => #t)
(check (every?-ec (: l 100)
                  (every?-ec (: i l)
                             (:let v (make-evector l 'foo))
                             (begin
                               (evector-fill! v 'bar i))
                             (equal? (evector->vector v)
                                     (vector-ec (: j l) (if (< j i) 'foo 'bar)))))
       => #t)
(check (let ([v (make-evector 20 'foo)])
         (evector-fill! v 'bar 10 15)
         (evector->vector v))
       =>(vector-ec (: i 20) (if (<= 10 i 14) 'bar 'foo)))

; evector-length
(check (every?-ec (: i 100)
                  (= (evector-length (make-evector i)) i))
       => #t)

; evector->list, list->evector
(check (evector->list (evector)) => (list))
(check (evector->list (evector 1 2 3)) => (list 1 2 3))
(check (every?-ec (let ([l (list-ec (: i 100) i)])
                    (equal? l (evector->list (list->evector l)))))
       => #t)

; evector-push!
(let ([v (vector-ec (: i 100) i)])
  (check (let ([ev (evector)])
           (do-ec (:vector x v)
                  (evector-push! ev x))
           (evector->vector ev))
         => v))

; evector-pop!
(let ([v (vector-ec (: i 100) i)])
  (check (let ([ev (vector->evector v)])
           (list-ec (:vector x v)
                    (evector-pop! ev)))
         => (reverse (list-ec (: x v) x))))


; evector-ref
(check (let ([v (evector 0 1 2 3)])
         (every?-ec (: i 4)
                    (= (evector-ref v i) i)))
       => #t)

; evector-set!
(check (let ([v (evector 0 1 2 3)])
         (every?-ec (: i 100)
                    (begin
                      (evector-set! v i 'foo))
                    (eq? (evector-ref v i) 'foo)))
       => #t)

; evector->vector, vector->evector
(check (evector->vector (evector)) => (vector))
(check (evector->vector (evector 1 2 3)) => (vector 1 2 3))
(check (every?-ec (let ([l (vector-ec (: i 100) i)])
                    (equal? l (evector->vector (vector->evector l)))))
       => #t)


; make-evector
(check (evector->list (make-evector 3)) => (list DFV DFV DFV))
(check (evector->list (make-evector 3 'a)) => (list 'a 'a 'a))
(check (evector-fill (make-evector 10 'foo)) => 'foo)
(check (evector->list (make-evector 0 'a #t)) => (list))
(check (evector->list (make-evector 0 'a #f)) => (list))

; set-evector-fill
(check (let ([v (make-evector 10 'foo)])
         (set-evector-fill! v 'bar)
         (evector-fill v))
       => 'bar)
(check (let ([v (make-evector 2 'a)])
         (set-evector-fill! v 'b)
         (set-evector-length! v 1000)
         (evector-ref v 999))
       => 'b)

; set-evector-length!
(check (every?-ec (: i 11 100)
                  (let ([v (make-evector i)])
                    (set-evector-length! v i)
                    (and (= (evector-length v) i)
                         (eqv? DFV (evector-ref v (- i 1))))))
       => #t)

; evector=?
(check 
 (and (evector=? (evector 1 2 3) (evector 1 2 3))
      (evector=? (evector 1 2 3) (evector 5 4 1)
                 (lambda (x y) (= (remainder x 2)
                                  (remainder y 2))))
      (not (evector=? (evector 1 2 3) (evector 3 2 3)))
      (not (evector=? (evector 1 2 3) (evector 1 2 7))))
 => #t)

; evector-map
(check (evector->list  (evector-map sqrt (evector 1 4 9 16)))
       => '(1 2 3 4))
(check (evector->list  (evector-map * (evector 1 2 3) (evector 4 5 6 7)))
       => '(4 10 18))
(check (evector->list  (evector-map * (evector 1 2 3 7) (evector 4 5 6)))
       => '(4 10 18))
(check (evector->list  (evector-map + (evector 1 2 3 7) (evector 4 5 6) (evector 7 8)))
       => '(12 15))

; evector-for-each
(check (let* ([sum 0] [add (lambda (x) (set! sum (+ sum x)))])
         (evector-for-each add (evector 1 2 3)) 
         sum)
       => 6)
(check (let* ([sum 0] [add (lambda (x y) (set! sum (+ sum (* x y))))])
         (evector-for-each add (evector 1 2 3) (evector 4 5 6))
         sum)
       => (+ (* 1 4) (* 2 5) (* 3 6)))
(check (let* ([sum 0] [add (lambda (x y z) (set! sum (+ sum (* x y z))))])
         (evector-for-each add (evector 1 2 3) (evector 4 5 6) (evector 7 8 9))
         sum)
       => (+ (* 1 4 7) (* 2 5 8) (* 3 6 9)))

(check (let* ([sum 0] [add (lambda (x y z) (set! sum (+ sum (* x y z))))])
         (evector-for-each add (evector 1 2 ) (evector 4 5 6) (evector 7 ))
         sum)
       => (+ (* 1 4 7) ))

; evector-copy

(check (evector=? (evector 1 2 3) (evector-copy (evector 1 2 3)))
       => #t)


;;; REPORT

(check-report)
