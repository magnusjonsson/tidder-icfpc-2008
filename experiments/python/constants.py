#!/usr/bin/env python

# Dunno why I'm doing this

# In metres
ROVER_RADIUS   = 0.5
HOME_RADIUS    = 5
MARTIAN_RADIUS = 0.4


DELIMITER  = ' '
TERMINATOR = ';'

ACC_STATES = STATE_BREAK, STATE_ROLL, STATE_ACC = "b", "-", "a"
DIR_STATES = STATE_HARDLEFT, STATE_LEFT, STATE_STRAIGHT, STATE_RIGHT, STATE_HARDRIGHT = "L", "l", "-", "r", "R"


MARTIAN, BOULDER, CRATER, HOME = "m", "b", "c", "h"

