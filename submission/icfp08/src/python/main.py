#! /usr/bin/env python

import socket

if __name__ == "__main__":
     host = "google.com"
     port = 80
     
     s = socket.socket()
     s.connect((host, port))
     
     # do work
     
     s.close()
