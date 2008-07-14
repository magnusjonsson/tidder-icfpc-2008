#lang scheme

(require "parser.scm")
(require "network.scm")
(require "messages.scm")
(require "ai.scm")

(define (do-it server port debug)
  (connect-server server port)
  (do () (#f)
    (let/ec escape
      (with-handlers* (((lambda (x) (and (not debug) (exn:fail? x)))
                        (lambda (v)
                          (printf "Catched ~a, trying to continue...~n" v)
                          (escape (void)))))
                      (when (message-available?)
                        (handle-message (get-message))))
      (sleep)))
  (disconnect))

(when (>= (vector-length (current-command-line-arguments)) 2)
  (do-it (vector-ref (current-command-line-arguments) 0)
         (string->number (vector-ref (current-command-line-arguments) 1))
         #f))
