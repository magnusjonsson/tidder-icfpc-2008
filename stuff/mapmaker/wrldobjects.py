#!/usr/bin/env python

import random
from config import *

random.seed()

def randfloat(low, high):
    return (random.random() * (high - low) + low)

# More-or-less automatically generated

def get_map():
    return \
    {
        "size" : get_size(),
        "timeLimit" : get_timeLimit(),
        "vehicleParams" : get_vehicleParams(),
        "martianParams" : get_martianParams(),
        "craters" : [ get_crater() for i in range(NCRATERS) ],
        "boulders" : [ get_boulder() for i in range (NBOULDERS) ],
        "runs" : [ get_run() for i in range(NRUNS) ]
    }

def get_vehicleParams():
    return \
    {
    "maxSpeed" : get_vehiclemaxSpeed(),
    "accel" : get_vehicleaccel(),
    "brake" : get_vehiclebrake(),
    "turn" : get_vehicleturn(),
    "hardTurn" : get_vehiclehardTurn(), 
    "rotAccel" : get_vehiclerotAccel(), 
    "frontView" : get_vehiclefrontView(), 
    "rearView" : get_vehiclerearView(),
    } 

def get_martianParams():
    return \
    {
    "maxSpeed" : get_martianmaxSpeed(),
    "accel" : get_martianaccel(),
    "brake" : get_martianbrake(),
    "turn" : get_martianturn(),
    "hardTurn" : get_martianhardTurn(), 
    "rotAccel" : get_martianrotAccel(), 
    "frontView" : get_martianfrontView(), 
    "rearView" : get_martianrearView(),
    }

def get_crater():
    return \
    {"x" : get_craterx(), "y" : get_cratery(), "r" : get_craterr()}

def get_boulder():
    return \
    {"x" : get_boulderx(), "y" : get_bouldery(), "r" : get_boulderr()}

def get_run():
    return \
    {
    "vehicle" : get_vehicle(),
    "enemies" : [ get_martian() for i in range(NMARTIANS) ]
    }

def get_vehicle():
    return \
    {
    "x" : get_vehiclex(),
    "y" : get_vehicley(),
    "dir" : get_vehicledir(),
    }

def get_martian():
    return \
    {
    "x" : get_martianx(),
    "y" : get_martiany(),
    "dir" : get_martiandir(),
    "speed" : get_martianspeed(),
    "view" : get_martianview()
    }

def get_size():
    return SIZE
    
def get_timeLimit():
    return TIMELIMIT

def get_vehiclemaxSpeed():
    return VEHICLE_MAXSPEED

def get_vehicleaccel():
    return VEHICLE_ACCEL
    
def get_vehiclebrake():
    return VEHICLE_BRAKE

def get_vehicleturn():
    return VEHICLE_TURN

def get_vehiclehardTurn():
    return VEHICLE_HARDTURN

def get_vehiclerotAccel():
    return VEHICLE_ROTACCEL

def get_vehiclefrontView():
    return VEHICLE_FRONTVIEW

def get_vehiclerearView():
    return VEHICLE_REARVIEW

def get_martianmaxSpeed():
    return randfloat(*MARTIAN_MAXSPEEDRANGE)

def get_martianaccel():
    return randfloat(*MARTIAN_ACCEL_RANGE)
    
def get_martianbrake():
    return randfloat(*MARTIAN_BRAKE_RANGE)

def get_martianturn():
    return randfloat(*MARTIAN_TURN_RANGE)

def get_martianhardTurn():
    return randfloat(*MARTIAN_HARDTURN_RANGE)

def get_martianrotAccel():
    return randfloat(*MARTIAN_ROTACCEL_RANGE)

def get_martianfrontView():
    return randfloat(*MARTIAN_FRONTVIEW_RANGE)

def get_martianrearView():
    return randfloat(*MARTIAN_REARVIEW_RANGE)

def get_craterx():
    return randfloat(-SIZE/2, SIZE/2)

def get_cratery():
    return randfloat(-SIZE/2, SIZE/2)

def get_craterr():
    return randfloat(*CRATER_RADIUSRANGE)

def get_boulderx():
    return randfloat(-SIZE/2, SIZE/2)

def get_bouldery():
    return randfloat(-SIZE/2, SIZE/2)

def get_boulderr():
    return randfloat(*CRATER_RADIUSRANGE)

# Might want to change this
def get_vehiclex():
    return randfloat(-SIZE/2, SIZE/2)

def get_vehicley():
    return randfloat(-SIZE/2, SIZE/2)

def get_vehicledir():
    return randfloat(-180, 180)

def get_martianx():
    return randfloat(-SIZE/2, SIZE/2)

def get_martiany():
    return randfloat(-SIZE/2, SIZE/2)

def get_martiandir():
    return randfloat(-180, 180)

def get_martianspeed():
    return randfloat(*MARTIAN_SPEEDRANGE)

def get_martianview():
    return randfloat(*MARTIAN_VIEWRANGE)
