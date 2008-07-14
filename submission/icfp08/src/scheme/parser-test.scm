#lang scheme

(require "parser.scm")
(require "network.scm")
(require "messages.scm")
(require "ai.scm")
(require (prefix-in gfx- "graphics.scm"))

(define (do-it server port debug)
  (when debug
    (gfx-init))
  (connect-server server port)
  (let/ec disconnected
    (do () (#f)
      (let/ec escape
        (with-handlers* (((lambda (x) (exn:fail:network? x))
                          (lambda (v)
                            (printf "Network failure, probably got disconnected.~n")
                            (disconnected)))
                         ((lambda (x) (and (not debug) (exn:fail? x)))                        
                          (lambda (v)
                            (printf "Caught ~a, trying to continue...~n" v)
                            (escape (void)))))
                        (when (message-available?)
                          (handle-message (get-message))))
        (sleep))))
  (disconnect)
  (gfx-quit))

(when (>= (vector-length (current-command-line-arguments)) 2)
  (do-it (vector-ref (current-command-line-arguments) 0)
         (string->number (vector-ref (current-command-line-arguments) 1))
         (>= (vector-length (current-command-line-arguments)) 3)))
