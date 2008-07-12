#lang scheme

(require "parser.scm")
(require "network.scm")
(require "messages.scm")
(require "ai.scm")

(connect-server "127.0.0.1" 17676)
(do () (#f)
  (when (message-available?)
    (handle-message (get-message)))
  (sleep))
(disconnect)
