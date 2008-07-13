#lang scheme

(require "parser.scm")
(require "network.scm")
(require "messages.scm")
(require "ai.scm")



(define (do-it server port)
  (connect-server server port)
  (do () (#f)
    (let/ec escape
      (with-handlers* ((exn:fail?
                        (lambda (v)
                          (printf "Catched ~a, trying to continue...~n" v)
                          (escape (void)))))
                      
                      (when (message-available?)
                        (handle-message (get-message))))
      (sleep)))
  (disconnect))

(when (>= (vector-length (current-command-line-arguments)) 2)
  (do-it (vector-ref (current-command-line-arguments) 0)
         (string->number (vector-ref (current-command-line-arguments) 1))))
