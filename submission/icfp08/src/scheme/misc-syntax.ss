#lang mzscheme

(provide push! pop! mergesort! while until min! max! inc! dec! dotimes)

(define-syntax push!
  (syntax-rules ()
      ((_ place value)
       (set! place (cons value place)))))

(define-syntax pop!
  (syntax-rules ()
    ((_ place)
     (begin0 (car place)
             (set! place (cdr place))))))

(require mzlib/list)

(define-syntax mergesort!
  (syntax-rules ()
    ((_ place <)
     (set! place (mergesort place <)))))

(define-syntax while
  (syntax-rules ()
    ((_ cond body ...)
     (let loop ()
       (if cond (begin body ... (loop)) (values))))))

(define-syntax until
  (syntax-rules ()
    ((_ cond body ...)
     (let loop ()
       (if cond (values) (begin body ... (loop)))))))

(define-syntax min!
  (syntax-rules ()
    ((_ place candidate)
     (set! place (min place candidate)))))

(define-syntax max!
  (syntax-rules ()
    ((_ place candidate)
     (set! place (max place candidate)))))

(define-syntax inc!
  (syntax-rules ()
    ((_ place) (set! (place (add1 place))))
    ((_ place increment) (set! (place (+ place increment))))))

(define-syntax dec!
  (syntax-rules ()
    ((_ place) (set! (place (sub1 place))))
    ((_ place decrement) (set! (place (- place decrement))))))


(define-syntax dotimes
  (syntax-rules ()
    ((_ (var num-times) body ...) (do ((var 0 (add1 var)))
                                    ((>= var num-times))
                                    body ...))))