#! /usr/bin/env python

import time
import sys
import os

from Connection import Connection

if __name__ == "__main__":
    host = "127.0.0.1"
    port = 17676
    
    connection = Connection(host, port)
    connection.write("a;")
    
    time.sleep(20)
    connection.close()
