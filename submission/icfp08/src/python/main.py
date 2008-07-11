#! /usr/bin/env python

import time
import sys
import os

import network

if __name__ == "__main__":
    host = "google.com"
    port = 80
    
    connection = network.Connection(host, port)
    connection.write("GET / HTTP/1.0\nHost: www.google.com\n\n")
    
    time.sleep(4)
    connection.close()
