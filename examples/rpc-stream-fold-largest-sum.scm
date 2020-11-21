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

(define (largest-sum return prev a b)
 (if (> prev 10)
  (return)
  (let ((sum (+ (apply + a) (apply + b))))
   (if (> prev sum) prev sum))))

(rpc-start)

(format #t "I'm master process. Received ~s ~%"
  (rpc-stream-fold largest-sum 0 stream-add-one stream-add-two))

(rpc-finalize)
