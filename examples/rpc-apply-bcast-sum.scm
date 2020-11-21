#!/usr/bin/env -S guile -s
!#

(add-to-load-path "..")
(use-modules (srfi srfi-1))
(use-modules (orca))

(define (sum-with-process-id a b) (+ a (rpc-worker-process-id) (fold + 0 b)))

(rpc-start)

(format #t "I'm master process. Received ~s ~%" (rpc-apply-bcast sum-with-process-id 10 '(5 3 2)))

(rpc-finalize)
