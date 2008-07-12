import sys

class Controller(object):
    def __init__(self, connection):
        connection.controller = self
        self.connection = connection
        self.buffer = ""
        self.brain = None
    
    def send_input(self, c):
        """Called by the Connection to send incoming data."""
        if c == ";":
            self.parse(self.buffer)
            self.buffer = ""
        else:
            self.buffer += c
    
    def send_command(self, acceleration, direction):
        c = ""
        
        if acceleration == -1:
            c += "b"
        elif acceleration == 1:
            c += "a"
        elif acceleration != 0:
            raise "invalid acceleration " + acceleration
        
        if direction == -1:
            c += "l"
        elif direction == 1:
            c += "r"
        elif direction != 0:
            raise "invalid direction " + direction
        
        if c != "":
            self.connection.send(c + ";")
    
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
        else:
            # TODO: parse the other message types
            pass
            
