import socket
from Queue import Queue
from threading import Thread
import time
import sys

class Connection(object):
    def __init__(self, host, port):
        self.socket = socket.socket()
        self.socket.connect((host, port))
        self.socket.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
        
        self.read_queue = Queue(0)
        self.write_queue = Queue(0)
        
        self.reader = ReaderThread(self)
        self.writer = WriterThread(self)
        self.reader.start()
        self.writer.start()
    
    def can_read(self):
        """Returns whether a subsequent call to read will return without
        blocking."""
        return not self.read_queue.empty()
    
    def read(self):
        """Reads a character and returns it; blocks until a new character is
        available."""
        return self.read_queue.get()
    
    def write(self, message):
        """Schedules a message to be written. Always returns immediately; the
        message will be sent in the background."""
        self.write_queue.put(message)
    
    def close(self):
        """Closes the underlying socket."""
        self.socket.close()

class ReaderThread(Thread):
    def __init__(self, connection):
        Thread.__init__(self)
        self.setDaemon(True) # ensure the thread doesn't zombify the app
        self.connection = connection
    
    def run(self):
        while True:
            try:
                data = self.connection.socket.recv(1)
            except socket.error, e:
                break
            if data:
                assert len(data) == 1
                c = data[0]
                
                self.connection.read_queue.put(c)
                
                # debug
                sys.stdout.write(c)

class WriterThread(Thread):
    def __init__(self, connection):
        Thread.__init__(self)
        self.setDaemon(True) # ensure the thread doesn't zombify the app
        self.connection = connection
    
    def run(self):
        while True:
            data = self.connection.write_queue.get()
            try:
                self.connection.socket.send(data)
            except socket.error, e:
                break
