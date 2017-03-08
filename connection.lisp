(in-package #:cl-user)
(defpackage #:jsonrpc/connection
  (:use #:cl)
  (:import-from #:jsonrpc/request-response
                #:request
                #:response
                #:response-id)
  (:import-from #:bordeaux-threads
                #:make-lock
                #:with-lock-held
                #:make-condition-variable
                #:condition-wait
                #:condition-notify)
  (:import-from #:dissect
                #:present)
  (:import-from #:event-emitter
                #:event-emitter)
  (:export #:connection
           #:*connection*
           #:connection-socket
           #:connection-request-callback
           #:add-message-to-queue
           #:add-message-to-outbox
           #:process-request
           #:next-request)
  (:documentation "jsonrpc/connection provides a class `connection' for holding data of each connections, like inbox and outbox."))
(in-package #:jsonrpc/connection)

(defvar *connection*)

(defclass connection (event-emitter)
  ((socket :initarg :socket
           :accessor connection-socket)
   (request-callback :initarg :request-callback
                     :accessor connection-request-callback)

   (request-queue :initform (make-array 0 :adjustable t :fill-pointer 0))

   (response-map :initform (make-hash-table :test 'equal))
   (request-lock :initform (bt:make-lock))
   (response-lock :initform (bt:make-lock))
   (response-callback :initform (make-hash-table :test 'equal))

   (receive-condvar :initform (bt:make-condition-variable))

   (outbox :initform (make-array 0 :adjustable t :fill-pointer 0))
   (outbox-lock :initform (bt:make-lock))))

(defgeneric add-message-to-queue (connection message)
  ;; batch
  (:method ((connection connection) (messages list))
    (if (typep (first messages) 'request)
        (with-slots (request-queue request-lock receive-condvar) connection
          (bt:with-lock-held (request-lock)
            (vector-push-extend messages request-queue))
          (bt:condition-notify receive-condvar))
        (dolist (response messages)
          (add-message-to-queue connection response)))
    (values))

  (:method ((connection connection) (message request))
    (with-slots (request-queue request-lock receive-condvar) connection
      (bt:with-lock-held (request-lock)
        (vector-push-extend message request-queue))
      (bt:condition-notify receive-condvar))
    (values))

  (:method ((connection connection) (message response))
    (let ((id (response-id message)))
      (unless id
        (warn "Unexpected response which has no id. Ignored.")
        (return-from add-message-to-queue))

      (with-slots (response-map
                   response-lock
                   response-callback) connection
        (bt:with-lock-held (response-lock)
          (let ((callback (gethash id response-callback)))
            (if callback
                (progn
                  (funcall callback message)
                  (remhash id response-callback))
                (setf (gethash id response-map) message))))))

    (values)))

(defun add-message-to-outbox (connection message)
  (check-type message request)
  (with-slots (outbox outbox-lock) connection
    (bt:with-lock-held (outbox-lock)
      (vector-push-extend message outbox))))

(defun set-callback-for-id (connection id callback)
  (with-slots (response-map
               response-callback
               response-lock) connection
    (bt:with-lock-held (response-lock)
      (let ((response (gethash id response-map)))
        (if response
            (progn
              (funcall callback response)
              (remhash id response-map))
            (setf (gethash id response-callback) callback))))
    (values)))

(defgeneric process-request (connection request)
  ;; batch request
  (:method ((connection connection) (requests list))
    (mapcar (lambda (request)
              (process-request connection request))
            requests))

  (:method ((connection connection) (request request))
    (let ((*connection* connection))
      (funcall (connection-request-callback connection) request))))

(defgeneric next-request (connection)
  (:method ((connection connection))
    (with-slots (request-queue
                 request-lock
                 receive-condvar) connection
      (when (= 0 (length request-queue))
        (bt:with-lock-held (request-lock)
          (bt:condition-wait receive-condvar request-lock)))
      (bt:with-lock-held (request-lock)
        (vector-pop request-queue)))))
