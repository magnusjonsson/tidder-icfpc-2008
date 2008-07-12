#lang scheme

(provide make insert! set! ref)

(require rnrs/arithmetic/bitwise-6)

; (hash key value)^size
(define (make size)
  (make-vector (* 3 size) #f))

; equal-hash-code produces rather bad hash codes for integers... this helps.
; 
; stolen from http://burtleburtle.net/bob/hash/integer.html
;
;uint32_t hash( uint32_t a)
;    a = (a ^ 61) ^ (a >> 16);
;    a = a + (a << 3);
;    a = a ^ (a >> 4);
;    a = a * 0x27d4eb2d;
;    a = a ^ (a >> 15);
;    return a;
;}
;
(define (scramble a)
  (let* ((a (bitwise-xor 61 a (bitwise-arithmetic-shift-right a 16)))
         (a (* a 9))
         (a (bitwise-xor a (bitwise-arithmetic-shift-right a 4)))
         (a (* a #x27d4eb2d))
         (a (bitwise-xor a (bitwise-arithmetic-shift-right a 15))))
    a))

(define (index v hash)
  (* 3 (modulo (scramble hash) (quotient (vector-length v) 3))))

(define (insert! v key value)
  (let* ((hash (equal-hash-code key))
         (i    (index v hash)))
    (vector-set! v i       hash)
    (vector-set! v (+ i 1) key)
    (vector-set! v (+ i 2) value)))

(define set! insert!)

(define (ref v key def)
  (let* ((hash (equal-hash-code key))
         (i    (index v hash)))
    (if (and (equal? hash (vector-ref v i))
             (equal? key  (vector-ref v (+ i 1))))
        (vector-ref v (+ i 2))
        (if (procedure? def) (def) def))))
