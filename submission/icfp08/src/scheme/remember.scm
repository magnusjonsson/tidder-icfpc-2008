#lang scheme

(require (prefix-in msg: "messages.scm"))
(provide remembered remember-objects remember-object
         print-remembered clear-remembered)

(define remembered (make-hash))

(define (remember-objects objects)
  (for-each remember-object objects))

(define (remember-object o)
  (when (and (msg:object? o)
             (not (eq? (msg:object-kind o) 'home-base))
             (not (hash-ref remembered o #f)))
    (hash-set! remembered o #t)
    ;update path here
    ))

(define (print-remembered)
  (printf "remembered objects:")
  (hash-for-each remembered
                 (lambda (key value)
                   (printf " ~a" key)))
  (printf "~n"))

(define (clear-remembered)
  (set! remembered (make-hash)))
