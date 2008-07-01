#lang mzscheme

(require "../scheme/search/astar.ss")
(require "../scheme/misc-syntax.ss")
(require scheme/base)
(require scheme/math)
(require scheme/match)

(define-struct pos (x y z) #:transparent)
(define (distance a b)
  (sqrt (+ (sqr (- (pos-x a) (pos-x b)))
           (sqr (- (pos-y a) (pos-y b)))
           (sqr (- (pos-z a) (pos-z b))))))
(define-struct hole (pos radius) #:transparent)
  (define (hole-distance h1 h2)
    (max 0 (- (distance (hole-pos h1) (hole-pos h2))
              (hole-radius h1)
              (hole-radius h2))))
(define-struct cheese (holes start goal) #:transparent)

(define (read-pos)
  (make-pos (read) (read) (read)))

(define (read-hole)
  (make-hole (read-pos) (read)))

(define (read-holes num-holes)
  (for/list ((i (in-range num-holes)))
            (read-hole)))

(define (read-cheese)
  (let ((num-holes (read)))
    (if (>= num-holes 0)
        (make-cheese (read-holes num-holes)
                     (read-pos)
                     (read-pos))
        #f)))

(define (read-problems)
  (let loop ((i 1))
    (let ((cheese (read-cheese)))
      (if (not cheese)
          null
          (cons (cons i cheese)
                (loop (add1 i)))))))

(define (solution< a b)
  #f)

(define (write-solution s)
  (match s
    ((cons id seconds)
     (printf "Cheese ~a: Travel time = ~a sec~n" id seconds))))

(define (write-solutions solutions)
  (for-each write-solution solutions))

(define (solve-cheese c)
  (define start (make-hole (cheese-start c) 0))
  (define goal  (make-hole (cheese-goal c) 0))
  (define (goal? state) (equal? state goal))
  (define (goal-distance state)
    (hole-distance state goal))
  (define (generate-moves! state yield!)
    (for ((hole (in-list (cons goal (cheese-holes c)))))
         (yield! (hole-distance state hole) hole hole)))

  (let/ec return
    (a* start goal? goal-distance generate-moves!
        (lambda (path distance)
          (return (round (* distance 10)))))))

(define (solve-problem p)
  (match p
    ((cons id cheese)
     (cons id (solve-cheese cheese)))))

(let* ((problems (with-input-from-file "problem2.input" read-problems))
       (solutions (time (map solve-problem problems))))
  (mergesort! solutions solution<)
  (write-solutions solutions))
