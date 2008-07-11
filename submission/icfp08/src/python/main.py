#! /usr/bin/env python

import time
import sys
import os

import network

if __name__ == "__main__":
    host = "127.0.0.1"
    port = 17676
    
    connection = network.Connection(host, port)
    connection.write("a;")
    
    time.sleep(20)
    connection.close()
