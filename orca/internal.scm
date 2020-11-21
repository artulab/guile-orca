;;; Copyright (C) 2020  Ahmet Artu Yildirim
;;;
;;; orca is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; orca is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with orca. If not, see <https://www.gnu.org/licenses/>.

(define-module (orca internal)
  #:use-module (orca config)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 q)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 eval-string)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 exceptions))

;;------------------------------------------------------------------------------

;;; Constants

;;------------------------------------------------------------------------------

;;; MPI Constants
(define MPI_COMM_WORLD #x44000000)
(define MPI_SUCCESS 0)
(define MPI_BYTE #x4c00010d)
(define MPI_STATUS_IGNORE (make-pointer 1))
(define MPI_STATUSES_IGNORE (make-pointer 1))

;;; Internal Constants
(define DEFAULT_ENCODING "utf-8")
(define MPI_OP_SEND 1)
(define MPI_OP_RECV 2)

;;------------------------------------------------------------------------------

;;; Object properties used internally

;;------------------------------------------------------------------------------

(define %mpi-request-buffer% (make-object-property))
(define %mpi-request-origin% (make-object-property))
(define %mpi-request-op% (make-object-property))
(define %mpi-receive-source% (make-object-property))

;;------------------------------------------------------------------------------

;;; Helper Functions

;;------------------------------------------------------------------------------

(define mpi-func
  (lambda* (return-type function-name arg-types)
    (pointer->procedure return-type
                        (dynamic-func function-name *libmpi*)
                        arg-types)))

(define-syntax-rule (define-mpi-foreign
                      name return-type func-name arg-types)
  (define name
    (mpi-func return-type func-name arg-types)))

(define (boolean->c-bool b)
  "Convert the boolean to a c boolean."
  (if b 1 0))

(define (c-bool->boolean b)
  "Convert the c boolean to boolean."
  (if (zero? b) #f #t))

(define (bytevector->int bv)
  (bytevector-sint-ref bv
                       0
                       (native-endianness)
                       (sizeof int)))

(define (bvlist->pointer lst size)
  (cond
   ((null? lst) %null-pointer)
   ((not (pair? lst)) (error "input is not a list"))
   (else (let* ((lst-length (length lst))
                (bv-target (make-bytevector (* size lst-length))))

           (do ((i 0 (1+ i)))
               ((>= i lst-length))
             (bytevector-copy! (list-ref lst i) 0 bv-target
                               (* i size)
                               size))
           (bytevector->pointer bv-target)))))

;;; MPI_Status structure

(define-record-type <mpi-status>
  (%make-mpi-status count cancelled mpi-source mpi-tag mpi-error)
  mpi-status?
  (count mpi-status-count set-mpi-status-count!)
  (cancelled mpi-status-cancelled set-mpi-status-cancelled!)
  (mpi-source mpi-status-source set-mpi-status-source!)
  (mpi-tag mpi-status-tag set-mpi-status-tag!)
  (mpi-error mpi-status-error set-mpi-status-error!))

(define* (make-mpi-status #:key
			  (count 0)
			  (cancelled 0)
			  (mpi-source 0)
			  (mpi-tag 0)
			  (mpi-error 0))
  (%make-mpi-status count cancelled mpi-source mpi-tag mpi-error))

(define mpi-status-types (list int int int int int))

(define (make-mpi-status-pointer)
  (make-c-struct
   mpi-status-types
   (list 0 0 0 0 0)))

(define (pointer->mpi-status pointer)
  (let ((lst (parse-c-struct pointer mpi-status-types)))
    (make-mpi-status #:count (list-ref lst 0)
                     #:cancelled (list-ref lst 1)
                     #:mpi-source (list-ref lst 2)
                     #:mpi-tag (list-ref lst 3)
                     #:mpi-error (list-ref lst 4))))

;;------------------------------------------------------------------------------

;;; MPI_Request

;;------------------------------------------------------------------------------

(define (make-mpi-request)
  (bytevector->pointer (make-bytevector (sizeof int))))

(define (mpi-request-list->pointer lst)
  (let ((bv-lst (map (lambda (ptr)
                       (pointer->bytevector ptr (sizeof int))) lst)))
    (bvlist->pointer lst (sizeof int))))

(define (mpi-request-list->buffer-list lst)
  (let ((q (make-q)))
    (let lp ((rest lst))
      (if (null? rest)
          (car q)
          (begin
            (when (= (%mpi-request-op% (car rest)) MPI_OP_RECV)
              (enq! q (bytevector->string
                       (%mpi-request-buffer% (car rest))
                       DEFAULT_ENCODING)))
            (lp (cdr rest)))))))

;;------------------------------------------------------------------------------

(define (exp->string s-exp)
  (call-with-output-string
    (lambda (port) (write s-exp port))))

(export exp->string)

;;------------------------------------------------------------------------------

(define (string->exp str)
  (call-with-input-string str (lambda (port) (read port))))

(export string->exp)

;;------------------------------------------------------------------------------

(define (eval-message message)
  (exp->string (eval (string->exp message) (interaction-environment))))

(export eval-message)

;;------------------------------------------------------------------------------

;;; MPI Bindings

;;------------------------------------------------------------------------------

(define-mpi-foreign %mpi-init
  int "MPI_Init" (list '* '*))

(define (mpi-init)
  "Initialize the MPI execution environment."
  (unless (= MPI_SUCCESS (%mpi-init %null-pointer %null-pointer))
    (error "failed to init mpi")))

(export mpi-init)

;;------------------------------------------------------------------------------

(define-mpi-foreign %mpi-finalize
  int "MPI_Finalize" '())

(define (mpi-finalize)
  "Terminates MPI execution environment."
  (unless (= MPI_SUCCESS (%mpi-finalize))
    (error "failed to finalize mpi")))

(export mpi-finalize)

;;------------------------------------------------------------------------------

(define-mpi-foreign %mpi-barrier
  int "MPI_Barrier" (list int))

(define (mpi-barrier)
  "Blocks until all processes in the communicator have reached this routine."
  (unless (= MPI_SUCCESS (%mpi-barrier MPI_COMM_WORLD))
    (error "failed to block")))

(export mpi-barrier)

;;------------------------------------------------------------------------------

(define-mpi-foreign %mpi-initialized
  int "MPI_Initialized" (list '*))

(define (mpi-initialized)
  "Check whether MPI has been initialized."
  (let* ((bv-flag (make-bytevector (sizeof int)))
         (result (%mpi-initialized (bytevector->pointer bv-flag))))
    (c-bool->boolean
     (bytevector->int bv-flag))))

(export mpi-initialized)

;;------------------------------------------------------------------------------

(define-mpi-foreign %mpi-comm-rank
  int "MPI_Comm_rank" (list int '*))

(define (mpi-rank)
  "Determine the rank of the calling process in the communicator."
  (let* ((bv-rank (make-bytevector (sizeof int)))
         (result (%mpi-comm-rank MPI_COMM_WORLD (bytevector->pointer bv-rank))))
    (if (= MPI_SUCCESS result)
	(bytevector->int bv-rank)
	(error "failed to get rank"))))

(export mpi-rank)

;;------------------------------------------------------------------------------

(define-mpi-foreign %mpi-comm-size
  int "MPI_Comm_size" (list int '*))

(define (mpi-size)
  "Return the size of the group associated with a communicator."
  (let* ((bv-size (make-bytevector (sizeof int)))
         (result (%mpi-comm-size MPI_COMM_WORLD (bytevector->pointer bv-size))))
    (if (= MPI_SUCCESS result)
	(bytevector->int bv-size)
	(error "failed to get size"))))

(export mpi-size)

;;------------------------------------------------------------------------------

(define-mpi-foreign %mpi-send
  int "MPI_Send" (list '* int int int int int))

(define (mpi-send-string dest message tag)
  "Send string message to destination process in a blocking mode."
  (let* ((bv-message (string->bytevector message DEFAULT_ENCODING))
         (bv-message-size (bytevector-length bv-message))
         (result (%mpi-send (bytevector->pointer bv-message)
                            bv-message-size
                            MPI_BYTE
                            dest
                            tag
                            MPI_COMM_WORLD)))
    (unless (= MPI_SUCCESS result)
      (error "failed to send message"))))

(export mpi-send-string)

;;------------------------------------------------------------------------------

(define-mpi-foreign %mpi-isend
  int "MPI_Isend" (list '* int int int int int '*))

(define (mpi-isend-string dest message tag)
  "Send string message to destination process in a nonblocking mode."
  (let* ((bv-message (string->bytevector message DEFAULT_ENCODING))
         (bv-message-size (bytevector-length bv-message))
         (mpi-request (make-mpi-request))
         (result (%mpi-isend (bytevector->pointer bv-message)
                             bv-message-size
                             MPI_BYTE
                             dest
                             tag
                             MPI_COMM_WORLD
                             mpi-request)))
    (if (= MPI_SUCCESS result)
	(begin
					; associate data with the request pointer
	  (set! (%mpi-request-origin% mpi-request) dest)
	  (set! (%mpi-request-op% mpi-request) MPI_OP_SEND)
	  mpi-request)
	(error "failed to send message"))))

(export mpi-isend-string)

;;------------------------------------------------------------------------------

(define-mpi-foreign %mpi-probe
  int "MPI_Probe" (list int int int '*))

(define (mpi-probe source tag)
  "Blocking test for a message."
  (let* ((status-p (make-mpi-status-pointer))
         (result (%mpi-probe source
                             tag
                             MPI_COMM_WORLD
                             status-p)))
    (if (= MPI_SUCCESS result)
	status-p
	(error "failed to probe for message"))))

;;------------------------------------------------------------------------------

(define-mpi-foreign %mpi-get-count
  int "MPI_Get_count" (list '* int '*))

(define (mpi-get-byte-count status-p)
  "Gets the number of bytes in the status"
  (let* ((bv-count (make-bytevector (sizeof int)))
         (result (%mpi-get-count status-p
                                 MPI_BYTE
                                 (bytevector->pointer bv-count))))
    (if (= MPI_SUCCESS result)
	(bytevector->int bv-count)
	(error "failed to get byte count"))))

;;------------------------------------------------------------------------------

(define-mpi-foreign %mpi-recv
  int "MPI_Recv" (list '* int int int int int '*))

(define (mpi-recv-string source tag)
  "Receive a string message from source process in a blocking mode."
  (let* ((status-p (mpi-probe source tag))
         (buf-count (mpi-get-byte-count status-p))
         (bv-buf (make-bytevector buf-count))
         (result (%mpi-recv (bytevector->pointer bv-buf)
                            buf-count
                            MPI_BYTE
                            source
                            tag
                            MPI_COMM_WORLD
                            MPI_STATUS_IGNORE)))
    (if (= MPI_SUCCESS result)
	(bytevector->string bv-buf DEFAULT_ENCODING)
	(error "failed to receive message"))))

(export mpi-recv-string)

;;------------------------------------------------------------------------------

(define-mpi-foreign %mpi-irecv
  int "MPI_Irecv" (list '* int int int int int '*))

(define (mpi-irecv-string source tag)
  "Begins a nonblocking receive."
  (let* ((status-p (mpi-probe source tag))
         (buf-count (mpi-get-byte-count status-p))
         (bv-buf (make-bytevector buf-count))
         (mpi-request (make-mpi-request))
         (result (%mpi-irecv (bytevector->pointer bv-buf)
                             buf-count
                             MPI_BYTE
                             source
                             tag
                             MPI_COMM_WORLD
                             mpi-request)))
    (if (= MPI_SUCCESS result)
	(begin
					; associate data with the request pointer
	  (set! (%mpi-request-buffer% mpi-request) bv-buf)
	  (set! (%mpi-request-origin% mpi-request) source)
	  (set! (%mpi-request-op% mpi-request) MPI_OP_RECV)
	  mpi-request)
	(error "failed to begin nonblocking receive"))))

(export mpi-irecv-string)

;;------------------------------------------------------------------------------

(define-mpi-foreign %mpi-wait
  int "MPI_Wait" (list '* '*))

(define (mpi-wait request)
  "Waits for an MPI request to complete."
  (let ((result (%mpi-wait request
                           MPI_STATUS_IGNORE)))
    (if (= MPI_SUCCESS result)
	(when (= (%mpi-request-op% request) MPI_OP_RECV)
          (bytevector->string (%mpi-request-buffer% request) DEFAULT_ENCODING))
	(error "failed to wait all"))))

(export mpi-wait)

;;------------------------------------------------------------------------------

(define-mpi-foreign %mpi-waitall
  int "MPI_Waitall" (list int '* '*))

(define (mpi-waitall requests)
  "Waits for all given MPI requests to complete."
  (cond
   ((null? requests) (error "input list is empty"))
   ((not (pair? requests)) (error "input is not a list"))
   (else (let* ((request-length (length requests))
                (result (%mpi-waitall request-length
                                      (mpi-request-list->pointer requests)
                                      MPI_STATUSES_IGNORE)))
           (if (= MPI_SUCCESS result)
               (mpi-request-list->buffer-list requests)
               (error "failed to wait all"))))))

(export mpi-waitall)
