#lang scheme

(provide (all-defined-out))

(define-struct init (dx dy ; meters
                     time-limit ; seconds
                     min-sensor max-sensor ; meters
                     max-speed ; meters/second
                     max-turn max-hard-turn ; degrees/second
                     )
  #:transparent)

(define-struct telemetry (time ; seconds
                          acceleration-control ; -1; braking, 0: rolling, 1: accelerating
                          turn-control         ; -2: hard right, -1 right, 0 straight, 1 left, 2 hard left
                          vehicle ; vehicle
                          seen ; (list (or object vehicle))
                          )
  #:transparent)

(define-struct vehicle (pos ; vec2 meters
                        dir ; degrees
                        speed ; meters/second
                        )
  #:transparent)

(define-struct object (kind ; (or 'boulder 'crater 'home-base)
                       pos ; vec2 meters
                       radius ; meters
                       )
  #:transparent)

(define-struct success (time)
  #:transparent)

(define-struct bump (time)
  #:transparent)

(define-struct failure (time
                        reason ; (or 'crater 'killed 'disconnected)
                        )
  #:transparent)

(define-struct end (time
                    score
                    )
  #:transparent)
