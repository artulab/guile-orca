#!/usr/bin/env -S guile -s
!#

(add-to-load-path "..")
(use-modules (orca))
(use-modules (ice-9 streams))

(define stream-add-two (make-stream (lambda (state)
                                      (if (> state (* 2 (rpc-worker-process-size)))
                                        '()
                                        (cons state (+ state 2))))
                         (rpc-worker-process-id)))

(define stream-add-one (make-stream (lambda (state)
                                     (if (> state (* 2 (rpc-worker-process-size)))
                                       '()
                                       (cons state (+ state 1))))
                        (rpc-worker-process-id)))

(define (display-all return a b) (display a) (newline) (display b) (newline) (newline))

(rpc-start)

(display "I'm master process.")
(newline)

(rpc-stream-for-each display-all stream-add-one stream-add-two)

(rpc-finalize)
