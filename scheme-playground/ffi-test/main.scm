#!/usr/bin/env mzscheme

#lang scheme

(require "test-module.scm")

(printf (foo))
(printf "\n")

(printf (number->string (native-bar)))
(printf "\n")

(display (sum-vector (vector 1 2 3 4)))
(newline)