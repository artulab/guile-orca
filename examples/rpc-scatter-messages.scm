#!/usr/bin/env -S guile -s
!#

(add-to-load-path "..")
(use-modules (orca))

(define (construct-message message) (format #f "process ~d ~a" (rpc-worker-process-id) message))

(rpc-start)

(format #t "I'm master process. Received ~s ~%"
  (rpc-scatter (list '(construct-message ("selam" ("mehmet"))) '(construct-message "hi")
                     '(construct-message "hallo") '(construct-message "konnichiwa"))))

(rpc-finalize)
