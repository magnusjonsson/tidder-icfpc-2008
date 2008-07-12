import sys

class Brain(object):
    def __init__(self, controller):
        controller.brain = self
        self.controller = controller
        self.s = 0
    
    def send_init(self, dx, dy, time_limit, min_sensor, max_sensor, max_speed, max_turn, max_hard_turn):
        pass
    
    def send_telemetry(self, time_stamp, vehicle_ctl_acc, vehicle_ctl_dir, vehicle_x, vehicle_y, vehicle_dir, vehicle_speed, objects):
        self.controller.set_acc(1)
        t = time_stamp / 1000
        if t >= self.s:
            d = -2 + 4 * (self.s % 2)
            self.controller.set_dir(d)
            self.s += 1
    
    def send_boulder(self, time_stamp):
        pass
    
    def send_crater(self, time_stamp):
        pass
    
    def send_killed(self, time_stamp):
        pass
    
    def send_end(self, time, score):
        self.s = 1
