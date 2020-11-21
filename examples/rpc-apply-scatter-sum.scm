#!/usr/bin/env -S guile -s
!#

(add-to-load-path "..")
(use-modules (srfi srfi-1))
(use-modules (orca))

(define (sum-it a b) (+ a b))

(rpc-start)

(format #t "I'm master process. Received ~s ~%"
  (rpc-apply-scatter sum-it '(10 20 30 40) '(1 2 3 4)))

(rpc-finalize)
