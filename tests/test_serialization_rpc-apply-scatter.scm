#!/usr/bin/env -S guile -s
!#

(add-to-load-path "..")
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-9 gnu))
(use-modules (srfi srfi-64))
(use-modules (orca))
(use-modules (rnrs bytevectors))

(define (test-payload test payload)
  (format #t "--Testing ~s: Payload ~a~%" test payload)
  (flush-all-ports)
  (case (string->symbol test)
    ((record) 'record)
    (else payload)))

(rpc-start)

(test-begin "rpc-apply-scatter-serialization-tests")

;; integer test

(define actual 10)

(define r (car (rpc-apply-scatter test-payload '("integer") `(,actual))))

(test-assert "integer test" (equal? r actual))

;; floadint point test

(set! actual 3.14159265358979323846)

(set! r (car (rpc-apply-scatter test-payload '("float") `(,actual))))

(test-assert "float test" (equal? r actual))

;; rational number test

(set! actual 11/7)

(set! r (car (rpc-apply-scatter test-payload '("rational") `(,actual))))

(test-assert "rational test" (equal? r actual))

;; boolean test

(set! actual #f)

(set! r (car (rpc-apply-scatter test-payload '("boolean") `(,actual))))

(test-assert "boolean test" (equal? r actual))

;; pair test

(set! actual '(1 . 2))

(set! r (car (rpc-apply-scatter test-payload '("pair") `(,actual))))

(test-assert "pair test" (equal? r actual))

;; list test

(set! actual '(1 2 3))

(set! r (car (rpc-apply-scatter test-payload '("list") `(,actual))))

(test-assert "list test" (equal? r actual))

;; null test

(set! actual '())

(set! r (car (rpc-apply-scatter test-payload '("null") `(,actual))))

(test-assert "null test" (equal? r actual))

;; list in list test

(set! actual '((1 2 3)))

(set! r (car (rpc-apply-scatter test-payload '("listinlist") `(,actual))))

(test-assert "listlist test" (equal? r actual))

;; vector test

(set! actual #(1 2 3))

(set! r (car (rpc-apply-scatter test-payload '("vector") `(,actual))))

(test-assert "vector test" (equal? r actual))

;; vector test

(set! actual #(1 2 3))

(set! r (car (rpc-apply-scatter test-payload '("vector") `(,actual))))

(test-assert "vector test" (equal? r actual))

;; array test

(set! actual #2((1 2 3) (4 5 6)))

(set! r (car (rpc-apply-scatter test-payload '("array") `(,actual))))

(test-assert "array test" (equal? r actual))

;; bytevector test

(set! actual #vu8(1 2 3))

(set! r (car (rpc-apply-scatter test-payload '("bytevector") `(,actual))))

(test-assert "bytevector test" (equal? r actual))

;; alist test

(set! actual '((foo . 1) (bar . 2)))

(set! r (car (rpc-apply-scatter test-payload '("alist") `(,actual))))

(test-assert "alist test" (equal? r actual))

(test-end "rpc-apply-scatter-serialization-tests")

(rpc-finalize)
