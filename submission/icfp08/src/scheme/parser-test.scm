#lang scheme

(require "parser.scm")
(require "network.scm")

(connect-server "localhost" 17676)
(do () (#f)
  (when (message-available?) (display (get-message)) (newline))
  (sleep))
(disconnect)
