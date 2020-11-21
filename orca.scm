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

(define-module (orca)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 q)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-4 gnu)
  #:use-module (ice-9 eval-string)
  #:use-module (ice-9 streams)
  #:use-module (orca config)
  #:use-module (orca internal))

;;------------------------------------------------------------------------------

;;; Internal data structures

;;------------------------------------------------------------------------------

(define DEFAULT_TAG 0)
(define MASTER_P_ID 0)
(define DATA_TAG 0)
(define MESSAGE_TAG 1)

(define stream-table (make-hash-table))

;;------------------------------------------------------------------------------

;;; Internal Messaging Functions

;;------------------------------------------------------------------------------

(define (%finalize-and-exit)
  (mpi-finalize)
  (exit 0))

(export %finalize-and-exit)

;;------------------------------------------------------------------------------

(define* (%accept-rpc-call source #:key (tag DEFAULT_TAG))
  (let ((message (mpi-recv-string source tag)))
    (mpi-send-string source (eval-message message) tag)))

(export %accept-rpc-call)

;;------------------------------------------------------------------------------

;;; Helper Functions

;;------------------------------------------------------------------------------

(define (any-process-completed? s-result)
  (cond
   ((null? s-result) #f)
   ((car s-result) (any-process-completed? (cdr s-result)))
   (else #t)))

;;------------------------------------------------------------------------------

(define (all-process-completed? s-result)
  (cond
   ((null? s-result) #t)
   ((car s-result) #f)
   (else (all-process-completed? (cdr s-result)))))

;;------------------------------------------------------------------------------

(define (initialize-mpi)
  (unless (mpi-initialized)
    (mpi-init)))

;;------------------------------------------------------------------------------

(define (quote-params params)
  (map (lambda (param)
	 (cond
	  ((null? param) '(quote ()))
	  ((and
	    (pair? param)
	    (not (eq? 'list (car param))))
	   (append '(quote) (list param)))
	  (else param))) params))

(define (quote-params-for-exp exp)
  `(,(car exp) ,@(quote-params (cdr exp))))

(define (quote-params-for-multiple-exp exps)
  (map (lambda (exp)
	 (quote-params-for-exp exp))
       exps))

;;------------------------------------------------------------------------------

(define (broadcast-rpc-message datum)
  (for-each
   (lambda (p-id)
     (mpi-isend-string p-id (exp->string datum) MESSAGE_TAG))
   (iota (1- (%process-size)) 1)))

;;------------------------------------------------------------------------------

(define (rpc-barrier)
  (mpi-barrier))

;;------------------------------------------------------------------------------

(define* (make-rpc-call dest s-exp #:key (tag DEFAULT_TAG))
  (let ((message (exp->string s-exp)))
    (mpi-send-string dest message tag)
    (string->exp (mpi-recv-string dest tag))))

;;------------------------------------------------------------------------------

(define* (make-rpc-async-call dest s-exp #:key (tag DEFAULT_TAG))
  (let ((message (exp->string s-exp)))
    (mpi-send-string dest message tag)
    (mpi-irecv-string dest tag)))

;;------------------------------------------------------------------------------

(define (wait-rpc-async request)
  (let ((message (mpi-wait request)))
    (string->exp message)))

;;------------------------------------------------------------------------------

(define (set-streams! ns streams)
  (let ((s-lst (hash-ref stream-table ns)))
    (if s-lst
        s-lst
        (hash-set! stream-table ns streams))))

(define (has-empty-stream? streams)
  (fold (lambda (current prev)
          (if prev
              prev
              (stream-null? current))) #f streams))

(define (collect-streams ns streams)
  (let ((result (map (lambda (s) (stream-car s)) streams)))
    (hash-set! stream-table ns (map (lambda (s) (stream-cdr s)) streams))
    result))


(define (%set-and-collect-streams ns streams)
  (let ((reg-stream (set-streams! ns streams)))
    (if (has-empty-stream? reg-stream)
	#f
	(collect-streams ns reg-stream))))

(export %set-and-collect-streams)

(define (%process-id)
  "Return the id of the calling process."
  (mpi-rank))

(define (%process-size)
  "Return the size of the process group."
  (mpi-size))

;;------------------------------------------------------------------------------

;;; Public Functions

;;------------------------------------------------------------------------------

(define (rpc-worker-process-id)
  "Return the id of the worker process."
  (mpi-rank))

(export rpc-worker-process-id)

;;------------------------------------------------------------------------------

(define (rpc-worker-process-size)
  "Return the number of the worker processes."
  (mpi-size))

(export rpc-worker-process-size)

;;------------------------------------------------------------------------------

(define (rpc-is-master-process)
  "Return #t if the process is master, #f otherwise"
  (= MASTER_P_ID (%process-id)))

(export rpc-is-master-process)

;;------------------------------------------------------------------------------

(define (rpc-make datum)
  (broadcast-rpc-message `(%accept-rpc-call ,MASTER_P_ID))

  (let ((my-message (exp->string datum)))
    (for-each (lambda (request) (mpi-wait request))
	      (map
	       (lambda (p-id)
		 (mpi-isend-string p-id my-message DATA_TAG))
	       (iota (1- (%process-size)) 1)))
    (cons (string->exp (eval-message my-message))
	  (map (lambda (p-id)
                 (string->exp (mpi-recv-string p-id DATA_TAG)))
	       (iota (1- (%process-size)) 1)))))

(export rpc-make)

;;------------------------------------------------------------------------------

(define (rpc-scatter lst-datum)
  (set! lst-datum (quote-params-for-multiple-exp lst-datum))

  (broadcast-rpc-message `(%accept-rpc-call ,MASTER_P_ID))

  (let lp ((cdr-datum (cdr lst-datum)) (p-id 1))
    (unless (nil? cdr-datum)
      (mpi-isend-string p-id (exp->string (car cdr-datum)) DATA_TAG)
      (lp (cdr cdr-datum) (1+ p-id))))
  (cons (string->exp (eval-message (exp->string (car lst-datum))))
	(map (lambda (p-id)
               (string->exp (mpi-recv-string p-id DATA_TAG)))
	     (iota (1- (%process-size)) 1))))

(export rpc-scatter)

;;------------------------------------------------------------------------------

(define-syntax rpc-apply-bcast
  (syntax-rules ()
    ((rpc-apply-bcast proc exp ...)
     (rpc-make `(proc ,@(quote-params (list exp ...)))))))

(export rpc-apply-bcast)

;;------------------------------------------------------------------------------

(define-syntax rpc-apply-scatter
  (syntax-rules ()
    ((rpc-apply-scatter proc exp ...)
     (rpc-scatter
      (map (lambda (params) `(proc ,@params)) (zip exp ...))))))

(export rpc-apply-scatter)

;;------------------------------------------------------------------------------

(define-syntax rpc-stream-map
  (syntax-rules ()
    ((rpc-stream-map proc stream1 ...)
     (call/cc
      (lambda (k)
	(let* ((ns (symbol->string (gensym "orca")))
               (datum `(%set-and-collect-streams ,ns (list stream1 ...)))
               (result '())
               (s-result '()))
          (while #t
            (set! s-result (rpc-make datum))
            (if (any-process-completed? s-result)
		(break)
		(set! result
		      (cons
		       (apply proc
			      (cons (lambda* (#:optional (ret #f))
				      (if ret
                                          (k ret)
                                          (k (reverse result))))
				    (apply zip s-result))) result))))
          (reverse result)))))))

(export rpc-stream-map)

;;------------------------------------------------------------------------------

(define-syntax rpc-stream-for-each
  (syntax-rules ()
    ((rpc-stream-for-each proc stream1 ...)
     (call/cc
      (lambda (k)
	(let* ((ns (symbol->string (gensym "orca")))
               (datum `(%set-and-collect-streams ,ns (list stream1 ...)))
               (s-result '()))
          (while #t
            (set! s-result (rpc-make datum))
            (if (any-process-completed? s-result)
		(break)
		(apply proc (cons k (apply zip s-result)))))
          #t))))))

(export rpc-stream-for-each)

;;------------------------------------------------------------------------------

(define-syntax rpc-stream-fold
  (syntax-rules ()
    ((rpc-stream-fold proc init stream1 ...)
     (call/cc
      (lambda (k)
	(let* ((ns (symbol->string (gensym "orca")))
               (datum `(%set-and-collect-streams ,ns (list stream1 ...)))
               (result '())
               (prev init)
               (s-result '()))
          (while #t
            (set! s-result (rpc-make datum))
            (if (any-process-completed? s-result)
		(break)
		(begin
		  (set! result
			(apply proc
			       (cons (lambda* (#:optional (ret #f))
				       (if ret
					   (k ret)
					   (k prev)))
				     (cons prev
					   (apply zip s-result)))))
		  (set! prev result))))
          result))))))

(export rpc-stream-fold)

;;------------------------------------------------------------------------------

(define (rpc-finalize)
  (broadcast-rpc-message '(%finalize-and-exit))
  (mpi-finalize))

(export rpc-finalize)

;;------------------------------------------------------------------------------

(define (rpc-start)
  (unless (rpc-is-master-process)
    (while #t
      (let ((message (mpi-recv-string MASTER_P_ID MESSAGE_TAG)))
	(eval (string->exp message) (interaction-environment))))))

(export rpc-start)

;;------------------------------------------------------------------------------

(initialize-mpi)
