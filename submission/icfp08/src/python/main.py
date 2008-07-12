#! /usr/bin/env python

import time
import sys
import os

from Connection import Connection
from Controller import Controller
from Brain import Brain

# The app is driven by the read loop in Connection.process().
# Characters are pushed into the Controller, which collects them
# to messages, parses these message and pushes them into the Brain.
# The Brain then updates its state and issues commands.

if __name__ == "__main__":
    host = "127.0.0.1"
    port = 17676
    
    connection = Connection(host, port)
    controller = Controller(connection)
    brain = Brain(controller)
    
    connection.process() # loops until the end of the game
