#! /usr/bin/env python

import socket
from Queue import Queue
from threading import Thread
import time
import sys

read_queue = Queue(0)

class ReaderThread(Thread):
    def __init__(self, s):
        Thread.__init__(self)
        self.socket = s
    
    def run(self):
        while True:
            try:
                d = self.socket.recv(1)
            except socket.error, e:
                break
            if d:
                assert len(d) == 1
                c = d[0]
                
                read_queue.put(c)
                
                # debug
                sys.stdout.write(c)

if __name__ == "__main__":
    host = "google.com"
    port = 80
    
    s = socket.socket()
    s.connect((host, port))
    s.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
    
    ReaderThread(s).start()
    
    s.send("GET / HTTP/1.0\nHost: www.google.com\n\n")
    time.sleep(4)
    
    s.close()
