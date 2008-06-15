#!/usr/bin/env mzscheme

#lang scheme

(require "test-module.scm")

(printf "~a~n" (foo))
(printf "~a~n" (native-bar))
(printf "~a~n" (sum-vector (vector 1 2 3 4)))
(apply printf "~a and ~a~n" (sum-and-reverse-vector (vector 1 2 3 4)))
