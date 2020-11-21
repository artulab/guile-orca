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

(define counter 0)

(define (sum-all return a b)
 (set! counter (1+ counter))
 (if (> counter 4)
  (return)
  (+ (apply + a) (apply + b))))

(rpc-start)

(format #t "I'm master process. Received ~s ~%"
  (rpc-stream-map sum-all stream-add-one stream-add-two))

(rpc-finalize)
