#!/usr/bin/env -S guile -s
!#

(add-to-load-path "..")
(use-modules (orca))

(rpc-start)

(display "Hello world from master process")
(newline)

(rpc-finalize)
