import sys
import socket
import time
import random
from constants import *
from math import *

_socket = None

def init_socket(host,port):
    global _socket
    s = socket.socket()
    s.setsockopt(socket.SOL_TCP, socket.TCP_NODELAY, 0)
    s.connect((host, port))
    _socket = s
    
def read_data():
    data,d = [], ""
    while d != ";":
        d = _socket.recv(1)
        #sys.stdout.write(d)
        data.append(d)
    print
    return "".join(data)

def send_data(d):
    _socket.send(d)
    
def close_socket():
    _socket.close()

# names as enums
dx, dy, time_limit, min_sensor, max_sensor ,max_speed, max_turn, max_hard_turn = range(8)
time_stamp, vehicle_ctl, vehicle_x, vehicle_y, vehicle_dir, vehicle_speed, objects = range(7)
object_kind ,object_x ,object_y ,object_r = range(4)
object_kind ,enemy_x, enemy_y ,enemy_dir ,enemy_speed= range(5)

def read_initialization():
    d = read_data()
    assert (d[0] == "I")
    d=d[1:-1].split()
    I= map(float,d)
    #print I
    return I

def parse_telemetry(d):
    assert (d[0] == "T")
    d = d[1:-1].split()
    T= [int(d[time_stamp]), d[vehicle_ctl],
        float( d[vehicle_x]), float( d[vehicle_y]),
        float(d[vehicle_dir]), float(d[vehicle_speed])]
    O = []
    if len(d) > 6:
        obx = d[6:]
        k = 0
        while k < len(obx):
            od = obx[k:]
            o = None
            if od[object_kind] != "m":
                o = obx[k], float(obx[k+1]), float(obx[k+2]), float(obx[k+3])
                k += 4
            else:
                o = obx[k], float(obx[k+1]), float(obx[k+2]), float(obx[k+3]), float(obx[k+4])
                k += 5
            O.append(o)
    T.append(O)
    ##print T            
    return T
    
def read_event():
    d = read_data()
    typ = d[0]
    if typ == "T":
        T = parse_telemetry(d)
        return typ,T
    return typ, []

def sq_distance_to_obj(T,obj):
    x1, y1 = T[vehicle_x], T[vehicle_y]
    x2, y2 = obj[object_x], obj[object_y] #works for martians too
    return (x1 - x2)**2 + (y1 - y2)**2 

def deg_to_rad(deg):
    return (deg/180.0) * pi


def rotate_point(x,y,a):
    #print "rot", (x,y,a)
    ca = cos(deg_to_rad(a))
    sa = sin(deg_to_rad(a))
    X = x * ca - y * sa
    Y = x * sa + y * ca
    return X,Y

def is_collision_course(T,obj):
    #print "*",T
    #print "**",obj
    xo,yo= obj[object_x], obj[object_y]
    #print "&", T[vehicle_dir]
    xn,yn=rotate_point(xo, yo, -T[vehicle_dir])
    if obj[object_kind] in ("c","h"):
        if xn > 0 and abs(yn) < ROVER_RADIUS :
            return True
        else:
            return False
    elif obj[object_kind] in ("m","b"):
        r = (obj[object_kind] == "m" and MARTIAN_RADIUS) \
                or obj[object_r]
        #print "%",yn , r , ROVER_RADIUS
        if xn > 0 and (abs(yn + r) < ROVER_RADIUS  or\
            abs(yn - r) < ROVER_RADIUS  or\
            ((yn + r > ROVER_RADIUS) and (yn - r < ROVER_RADIUS))):
            return True
        else:
            return False
    assert(0)

        
def turn_home(T):
    xh, yh = -T[vehicle_x], -T[vehicle_y]
    xnh, ynh = rotate_point(xh, yh, -T[vehicle_dir])
    if ynh > 0:
        if xnh > 0:
            return "l;"
        else:
            return "l;l;"
    else:
        if xnh > 0:
            return "r;"
        else:
            return "r;r;"

    
def estimate_dangers(T):
    moves = []
    flag_collision = False
    for obj in T[objects]:
        #print obj
        if obj[object_kind] in ("b", "c", "m"):
            if is_collision_course(T, obj):
                turn = turn_home(T)
                print T, obj, turn
                moves.append( (sq_distance_to_obj(T,obj),"b"+turn))
                flag_collision=True  
    if not flag_collision:
        moves = [(1,"a;"+turn_home(T))]
    return moves

def send_move(T):
    #moves = (";", "a;", "b;", "l;", "r;", "al;", "ar;", "bl;", "br;")
    moves = estimate_dangers(T)
    #print "moves", moves
    send_data(max(moves, key=lambda x: -x[0])[1])
    
def play_round(I):
    while True:
        #time.sleep(0)
        while True:
            typ,Ev = read_event()
            if typ in ("C","K","S", "B"):
                ##print typ, Ev
                continue
            elif typ == "E":
                return
            elif typ == "T":
                send_move(Ev)
                break
            else:
                ##print typ, Ev
                assert(0)
    
def play(host,port):
    init_socket(host,port)
    I = read_initialization()
    while True:
        play_round(I)

if __name__ == "__main__":
    host = "localhost"
    port = 17676
    play(host, port)
    
    
    
