#lang scheme

(provide (all-defined-out))

(define-struct init (dx dy ; meters
                     time-limit ; seconds
                     min-sensor max-sensor ; meters
                     max-speed ; meters/second
                     max-turn max-hard-turn ; degrees/second
                     ))

(define-struct telemetry (time-stamp ; seconds
                          acceleration-control ; -1; braking, 0: rolling, 1: accelerating
                          turn-control         ; -2: hard left, -1 left, 0 straight, 1 right, 2 hard right
                          vehicle ; vehicle
                          seen ; (list (or object vehicle))
                          ))

(define-struct vehicle (x y ; meters
                        dir ; degrees
                        speed ; meters/second
                        ))

(define-struct object (kind ; (or 'boulder 'crater 'home-base)
                       x y ; meters
                       radius ; meters
                       ))

(define-struct success ())

(define-struct failure (reason ; (or 'crash 'killed 'disconnected)
                    ))

(define-struct end ())
