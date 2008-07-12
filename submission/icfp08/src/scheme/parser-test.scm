#lang scheme

(require "parser.scm")
(require "network.scm")
(require "messages.scm")
(require "ai.scm")
(require "path.scm")
(require "remember.scm")

; hack to break circular dependency;
; not yet sure how to structure this properly
(set-update-path! update-path)

(connect-server "127.0.0.1" 17676)
(do () (#f)
  (when (message-available?)
    (handle-message (get-message)))
  (sleep))
(disconnect)
