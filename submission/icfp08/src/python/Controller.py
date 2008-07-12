import sys

class Controller(object):
    def __init__(self, connection):
        connection.controller = self
        self.connection = connection
        self.buffer = ""
        self.brain = None
        self.reset()
    
    def reset(self):
        self.acc = 0
        self.dir = 0
    
    def set_acc(self, acc):
        self.set_acc_dir(acc, self.dir)
    
    def set_dir(self, dir):
        self.set_acc_dir(self.acc, dir)
    
    def set_acc_dir(self, acc, dir):
        msg = ""
        
        acc = min(1, max(-1, acc))    
        if acc > self.acc:
            msg += "a;" * (acc - self.acc)
        elif acc < self.acc:
            msg += "b;" * (self.acc - acc)
        self.acc = acc
        
        dir = min(2, max(-2, dir))    
        if dir > self.dir:
            msg += "l;" * (dir - self.dir)
        elif dir < self.dir:
            msg += "r;" * (self.dir - dir)
        self.dir = dir
        
        if msg:
            self.connection.send(msg)
    
    def send_input(self, c):
        """Called by the Connection to send incoming data."""
        if c == ";":
            self.parse(self.buffer)
            self.buffer = ""
        else:
            self.buffer += c
        
    def parse(self, message):
        if not self.brain:
            return
        
        parts = message.split()
        
        if parts[0] == "I":
            self.brain.send_init(
                    float(parts[1]), # dx
                    float(parts[2]), # dy
                    float(parts[3]), # time_limit
                    float(parts[4]), # min_sensor
                    float(parts[5]), # max_sensor
                    float(parts[6]), # max_speed
                    float(parts[7]), # max_turn
                    float(parts[8]) # max_hard_turn
                )
        elif parts[0] == "T":
            # TODO: parse objects
            
            self.brain.send_telemetry(
                    float(parts[1]), # time_stamp
                    0, # TODO: vehicle_ctl_acc
                    0, # TODO: vehicle_ctl_dir
                    float(parts[3]), # vehicle_x
                    float(parts[4]), # vehicle_y
                    float(parts[5]), # vehicle_dir
                    float(parts[6]), # vehicle_speed
                    [] # objects
                )
        elif parts[0] == "E":
            self.reset()
            self.brain.send_end(float(parts[1]), float(parts[2]))
        else:
            # TODO: parse the other message types
            pass
            
