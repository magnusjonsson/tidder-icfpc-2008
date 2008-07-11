#!/usr/bin/env mzscheme

#lang scheme

(require scheme/tcp)
(provide connect-server char-available? get-char send-string disconnect)

(define in  #f)
(define out #f)

(define (connect-server host port)
  (let-values (((my-in my-out) (tcp-connect host port)))
    (set! in   my-in)
    (set! out  my-out)
    (file-stream-buffer-mode  in 'none)
    (file-stream-buffer-mode out 'none)))

(define (disconnect)
  (close-output-port out)
  (close-input-port  in))

(define (char-available?)
  (char-ready? in))

(define (get-char)
  (let ((c (read-char in)))
    (when (eof-object? c) (exit))
    c))

(define (send-string string)
  (write-string string out))
