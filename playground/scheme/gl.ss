#lang scheme

(require scheme/gui)
(require sgl)

(define frame (new frame% [label "GL test"]
                   [width 512]
                   [height 512]))
(define my-canvas%
  (class canvas%
    (override on-paint)
    (define (on-paint) (gl-thunk))
    
    (super-instantiate ())))

(define canvas (new my-canvas% [parent frame]))
(send frame show #t)

(define-syntax gl-block
  (syntax-rules ()
    ((_ what body ...)
     (begin (gl-begin what)
            body ...
            (gl-end)))))

(define (gl-thunk)
  (send canvas with-gl-context
        (lambda ()
          (gl-clear-color 0.0 0.0 0.0 0.0)
          (gl-clear 'color-buffer-bit)
          (gl-block 'triangles
                    (gl-color 1.0 0.0 0.0)
                    (gl-vertex 0.5 0.0 0.0)
                    (gl-vertex 0.0 0.5 0.0)
                    (gl-vertex -0.5 0.0 0.0))
          (gl-flush)
          ))
  (send canvas swap-gl-buffers))