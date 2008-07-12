#lang scheme

(require (prefix-in msg: "messages.scm"))
(provide remembered remember-objects remember-object
         print-remembered clear-remembered)
 
(define remembered (make-hash))

(define (remember-objects objects)
  (for-each remember-object objects))

(define (remember-object o)
  (cond
    ((msg:object? o) (hash-set! remembered o #t))))

(define (print-remembered)
  (printf "remembered objects:")
  (hash-for-each remembered
                 (lambda (key value)
                   (printf " ~a" key)))
  (printf "~n"))

(define (clear-remembered)
  (set! remembered (make-hash)))
