#!/usr/bin/env -S guile -s
!#

(add-to-load-path "..")
(use-modules (orca))

(define (process-id-message) (format #f "process ~a" (rpc-worker-process-id)))

(rpc-start)

(format #t "I'm master process. Received ~s ~%" (rpc-make '(process-id-message)))

(rpc-finalize)
