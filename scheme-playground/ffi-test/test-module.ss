#lang scheme

(require scheme/foreign)
(unsafe!)

(provide foo)
(define (foo) "Hello World!")
