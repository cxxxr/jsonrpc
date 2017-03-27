(in-package #:cl-user)
(defpackage #:jsonrpc/connection
  (:use #:cl)
  (:import-from #:jsonrpc/request-response
                #:request
                #:response
                #:response-id)
  (:import-from #:bordeaux-threads
                #:make-lock
                #:with-lock-held)
  (:import-from #:dissect
                #:present)
  (:import-from #:event-emitter
                #:event-emitter)
  (:import-from #:chanl)
  (:export #:connection
           #:*connection*
           #:connection-socket
           #:connection-request-callback
           #:add-message-to-queue
           #:process-request
           #:connection-request-queue)
  (:documentation "jsonrpc/connection provides a class `connection' for holding data of each connections, like inbox."))
(in-package #:jsonrpc/connection)

(defvar *connection*)

(defclass connection (event-emitter)
  ((socket :initarg :socket
           :accessor connection-socket)
   (request-callback :initarg :request-callback
                     :accessor connection-request-callback)

   (request-queue :initform (make-instance 'chanl:unbounded-channel)
                  :accessor connection-request-queue)

   (response-map :initform (make-hash-table :test 'equal))
   (response-lock :initform (bt:make-lock))
   (response-callback :initform (make-hash-table :test 'equal))))

(defgeneric add-message-to-queue (connection message)
  ;; batch
  (:method ((connection connection) (messages list))
    (if (typep (first messages) 'request)
        (chanl:send (slot-value connection 'request-queue) messages)
        (dolist (response messages)
          (add-message-to-queue connection response)))
    (values))

  (:method ((connection connection) (message request))
    (chanl:send (slot-value connection 'request-queue) message)
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
