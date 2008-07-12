import socket
from Queue import Queue
from threading import Thread
import time
import sys

class Connection(object):
    def __init__(self, host, port):
        self.host = host
        self.port = port
        self.controller = None
        self.socket = None
    
    def process(self):
        s = socket.socket()
        s.connect((self.host, self.port))
        s.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
        self.socket = s
        
        read_loop(self)
    
    def send(self, message):
        sys.stdout.write(message[0:-1])
        sys.stdout.flush
        self.socket.send(message)
    
    def close(self):
        """Closes the underlying socket."""
        self.socket.close()

def read_loop(connection):
    while True:
        try:
            data = connection.socket.recv(1)
        except socket.error, e:
            break
        if data:
            assert len(data) == 1
            c = data[0]
            
            if connection.controller:
                connection.controller.send_input(c)
            
            # debug
            if c >= 'A' and c <= 'Z':
                sys.stdout.write(c)
                sys.stdout.flush()
