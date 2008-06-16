#!/usr/bin/env mzscheme

#lang scheme

(require "test-module.scm")

(printf "foo: ~a~n" (foo))
(printf "bar: ~a~n" (native-bar))

(printf "sum-vector: ~a~n" (sum-vector (vector 1 2 3 4)))
(printf "sum-float-vector: ~a~n" (sum-float-vector (vector 1.0 1.5 100.0)))

(define v (vector 1.0 2.0 3.0 4.0))
(printf "original: ~a~n" v)
(printf "squared : ~a~n" (square-float-vector v))
(printf "original: ~a~n" v)
