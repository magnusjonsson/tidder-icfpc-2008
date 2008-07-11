#lang scheme

(provide message-available? get-message)
(require (prefix-in net: "network.scm"))
(require (prefix-in msg: "messages.scm"))
(require "misc-syntax.ss")
(require mzlib/pregexp)
(require scheme/match)
(require (only-in rnrs/base-6 assert))

(define chars '())

(define (message-finished?)
  (and (not (empty? chars))
       (or (equal? (car chars) #\;)
           (equal? (car chars) 'eof))))

(define (message-available?)
  (or (message-finished?)
      (and (net:char-available?)
           (begin
             (set! chars (cons (net:get-char) chars))
             (message-available?)))))

(define (get-message)
  (and (message-finished?)
       (if (equal? (car chars) 'eof)
           'eof
           (parse (list->string (reverse (begin0 chars (set! chars '()))))))))

(define (parse msg)
  (let ((tokens (filter (lambda (x) (not (zero? (string-length x))))
                        (pregexp-split "[ \n;]" msg))))
    (define (n) (string->number (pop! tokens)))
    (define (t) (/ (n) 1000)) ; time in ms -> seconds
    (define (accel) (case (string-ref (car tokens) 0)
                      ((#\a) 1)
                      ((#\-) 0)
                      ((#\b) -1)))
    (define (turn) (case (string-ref (pop! tokens) 1)
                     ((#\L) -2)
                     ((#\l) -1)
                     ((#\-) 0)
                     ((#\r) 1)
                     ((#\R) 2)))
    (define (vehicle) (msg:make-vehicle (n) (n) (n) (n)))
    (define (seen) (match (pop! tokens)
                     ("b" (msg:make-object 'boulder (n) (n) (n)))
                     ("c" (msg:make-object 'crater (n) (n) (n)))
                     ("h" (msg:make-object 'home-base (n) (n) (n)))
                     ("m" (vehicle))))
    (define (many x)
      (if (not (null? tokens))
          (cons (x) (many x))
          '()))

    (match (pop! tokens)
      ("I" (msg:make-init (n) (n) (t) (n) (n) (n) (n) (n)))
      ("T" (msg:make-telemetry (t) (accel) (turn) (vehicle) (many seen)))
      ("B" (msg:make-crash (t))
      ("C" (msg:make-failure (t) 'crater))
      ("K" (msg:make-failure (t) 'killed))
      ("S" (msg:make-success (t)))
      ("E" (msg:make-end (t) (n))))))


(define (test-parse)
  (define (-> input output)
    (let ((actual-output (parse input)))
      (if (equal? (parse input) output)
          #t
          (begin
            (printf "test-parse: (parse ~s) should give ~s, but actually gives ~s"
                    input output actual-output)))))
  
  (->
   "T 3450 aL -234.040 811.100 47.5 8.450 b -220.000 750.000 12.000 m -240.000 812.000 90.0 9.100 ;"
   (msg:make-telemetry 3450/1000 1 -2
                       (msg:make-vehicle -234.040 811.100 47.5 8.450)
                       (list (msg:make-object 'boulder -220.000 750.000 12.000)
                             (msg:make-vehicle -240.000 812.000 90.0 9.100))))
  (->
   "I 50.000 40.000 100 2.000 3.000 5.000 60.0 90.0"
   (msg:make-init 50.0 40.0 100/1000 2.0 3.0 5.0 60.0 90.0))
  
  (-> "B 50 ;" (msg:make-crash 50/1000))
  (-> "C 50 ;" (msg:make-failure 50/1000 'crater))
  (-> "K 50 ;" (msg:make-failure 50/1000 'killed))
  (-> "S 50 ;" (msg:make-success 50/1000))
  (-> "E 50 10000 ;" (msg:make-end 50/1000 10000))
  )