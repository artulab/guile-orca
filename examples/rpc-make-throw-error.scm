#!/usr/bin/env -S guile -s
!#

(add-to-load-path "..")
(use-modules (orca))

(define (throw-error) (error "something bad happened"))

(rpc-start)

(format #t "I'm master process. Received ~s ~%" (rpc-make '(throw-error)))

(rpc-finalize)
