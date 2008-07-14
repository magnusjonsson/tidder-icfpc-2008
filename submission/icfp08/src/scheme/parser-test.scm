#lang scheme

(require "parser.scm")
(require "network.scm")
(require "messages.scm")
(require "ai.scm")
(require (prefix-in gfx- "graphics.scm"))

(define (do-it server port debug gfx-program)
  (when gfx-program
    (gfx-init "../../../../stuff/graphics/g"))
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
                        (if (message-available?)
                          (handle-message (get-message))
                          (handle-message (make-inbox-empty))))
        (sleep))))
  (disconnect)
  (when (gfx-on?)
    (gfx-quit)))

(define (getarg i default)
  (let ((args (current-command-line-arguments)))
    (if (< i (vector-length args))
        (vector-ref args i)
        default)))

(when (>= (vector-length (current-command-line-arguments)) 2)
  (do-it (getarg 0 "localhost")
         (string->number (getarg 1 "17676"))
         (getarg 2 #f)
         (getarg 3 #f)))
