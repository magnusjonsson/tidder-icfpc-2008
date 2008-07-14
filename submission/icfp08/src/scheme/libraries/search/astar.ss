#lang scheme

(require (prefix-in prio: "../queue/priority-queue.ss"))
(provide a*)

(define-struct item (prio depth state path))

(define (a* start goal? heuristic generate-moves! success!)
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
             (when (= depth best-depth)
               (if (goal? state)
                   (success! path depth)
                   (generate-moves! state
                                    (lambda (cost desc next-state)
                                      (add! (+ depth cost)
                                            next-state
                                            (cons desc path)))))))))))
    (add! 0 start '())
    (let loop ()
      (unless (prio:empty? frontier)
        (try-next!)
        (loop)))))