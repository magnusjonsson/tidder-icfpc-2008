#lang scheme

(require scheme/foreign)
(unsafe!)

(provide foo)
(define (foo) "Hello World!")

(define native-lib (ffi-lib "native.so"))
(define native-bar (get-ffi-obj "bar" native-lib (_fun -> _int)))
(provide native-bar)
