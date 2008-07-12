import sys

class Brain(object):
    def __init__(self, controller):
        controller.brain = self
        self.controller = controller
    
    def send_init(self, dx, dy, time_limit, min_sensor, max_sensor, max_speed, max_turn, max_hard_turn):
        pass
    
    def send_telemetry(self, time_stamp, vehicle_ctl_acc, vehicle_ctl_dir, vehicle_x, vehicle_y, vehicle_dir, vehicle_speed, objects):
        self.controller.send_command(1, 1)
    
    def send_boulder(self, time_stamp):
        pass
    
    def send_crater(self, time_stamp):
        pass
    
    def send_killed(self, time_stamp):
        pass
    
    def send_end(self, time, score):
        pass
