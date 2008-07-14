#lang scheme

(require (prefix-in prio: "../queue/priority-queue.ss"))
(provide a*)

(define-struct item (prio depth state path))

; returns a function that when called returns
; #f if there is no solution
; #t if calling it again may yield a solution
; (path cost) if a solution is found
(define (a* start goal? heuristic generate-moves!)
  (let ((depth-hash (make-hash))
        (frontier   (prio:make (lambda (a b)
                                 (< (item-prio a) (item-prio b))))))
    (define (add! depth state path)
      (let ((prev-depth (hash-ref depth-hash state #f)))
        (when (or (not prev-depth)
                  (< depth prev-depth))
          (hash-set! depth-hash state depth)
          (prio:insert! frontier (make-item (+ depth (heuristic state))
                                            depth state path)))))
    (define (try-next!)
      (let* ((first-item  (prio:extract-min! frontier)))
        (match first-item
          ((struct item (_ depth state path))
           (let ((best-depth (hash-ref depth-hash state #f)))
             (if (> depth best-depth)
                 #t
                 (if (goal? state)
                     (list path depth)
                     (begin
                       (generate-moves! state
                                        (lambda (cost desc next-state)
                                          (add! (+ depth cost)
                                                next-state
                                                (cons desc path))))
                       #t))))))))
    (add! 0 start '())
    (lambda ()
      (if (prio:empty? frontier)
          #f
          (try-next!)))))
