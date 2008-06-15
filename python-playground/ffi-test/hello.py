#! /usr/bin/env python

from ctypes import *

native = cdll.LoadLibrary("native.so")

print native.bar()

def c_array(c_type, p_list):
	c_array_type = c_type * len(p_list)
	return c_array_type(*p_list)

foo = [1, 2, 3, 4, 5]
print native.sum_array(c_array(c_int, foo), len(foo))
