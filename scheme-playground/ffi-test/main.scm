#!/usr/bin/env mzscheme

#lang scheme

(require "test-module.scm")

(printf (foo))
(printf "\n")

(printf (number->string (native-bar)))
(printf "\n")
