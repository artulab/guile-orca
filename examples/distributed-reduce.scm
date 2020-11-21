#!/usr/bin/env -S guile -s
!#

(add-to-load-path "..")

(use-modules (orca))
(use-modules (srfi srfi-1))
(use-modules (ice-9 regex))
(use-modules (ice-9 ftw))
(use-modules (ice-9 textual-ports))

(define proc max)
(define init-value 0)

(define (read-db-numbers db-lst)
  (define db-numbers '())

 (for-each (lambda (db)
            (let ((port (open-file (format #f "./numbers-db/~a" db) "r")))
             (let lp ((line (get-line port)))
              (if (not (eof-object? line))
               (begin
                (set! db-numbers (cons (string->number line) db-numbers))
                (lp (get-line port)))
               (close-port port)))))
           db-lst) db-numbers)

(define (apply-distributed-proc db-lst)
 (reduce proc init-value (read-db-numbers db-lst)))

(rpc-start)

(define (partition-db-lists)
  (define db-lst (scandir "./numbers-db" (lambda (f) (string-match "^.*\\.txt$" f))))
  (define db-per-worker (euclidean-quotient (length db-lst) (rpc-worker-process-size)))
  (define dbs '())

  (when (= db-per-worker 0)
    (error "insufficient db size"))

  (let lp ((i (1- (rpc-worker-process-size))))
   (if (>= i 1)
     (begin
       (set! dbs (cons (take db-lst db-per-worker) dbs))
       (set! db-lst (drop db-lst db-per-worker))
       (lp (1- i)))
     (begin
       (set! dbs (cons db-lst dbs))
       (set! dbs (reverse dbs)))))
  (map (lambda (k) (cons 'list k)) dbs))

(format #t "result is ~d~%"
  (reduce proc init-value
    (rpc-apply-scatter apply-distributed-proc (partition-db-lists))))

(rpc-finalize)
