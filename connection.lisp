(defpackage #:jsonrpc/connection
  (:use #:cl)
  (:import-from #:jsonrpc/request-response
                #:request
                #:response
                #:response-id)
  (:import-from #:bordeaux-threads
                #:make-condition-variable
                #:make-recursive-lock
                #:with-recursive-lock-held
                #:condition-wait
                #:condition-notify
                #:*default-special-bindings*)
  (:import-from #:dissect
                #:present)
  (:import-from #:chanl)
  (:import-from #:vom)
  (:export #:connection
           #:*connection*
           #:wait-for-ready
           #:connection-stream
           #:connection-request-callback
           #:add-message-to-queue
           #:add-message-to-outbox
           #:process-request
           #:connection-request-queue
           #:connection-outbox)
  (:documentation "jsonrpc/connection provides a class `connection' for holding data of each connections, like inbox and outbox."))
(in-package #:jsonrpc/connection)

(defvar *connection*)

(defclass connection ()
  ((stream :initarg :stream
           :accessor connection-stream)
   (request-callback :initarg :request-callback
                     :accessor connection-request-callback)

   (request-queue :initform (make-instance 'chanl:unbounded-channel)
                  :accessor connection-request-queue)

   (response-map :initform (make-hash-table :test 'equal)
                 :reader connection-response-map)
   (response-lock :initform (bt:make-recursive-lock "jsonrpc/connection response-lock")
                  :reader connection-response-lock)
   (response-callback :initform (make-hash-table :test 'equal)
                      :reader connection-response-callback)

   (condvar :initform (bt:make-condition-variable))
   (condlock :initform (bt:make-recursive-lock))
   (outbox :initform (make-instance 'chanl:unbounded-channel)
           :accessor connection-outbox)))

(defgeneric send-and-notify (connection channel message)
  (:method ((connection connection) channel message)
    (bt:with-recursive-lock-held ((slot-value connection 'condlock))
      (chanl:send channel message)
      (bt:condition-notify (slot-value connection 'condvar)))))

(defmethod wait-for-ready ((connection connection))
  (bt:with-recursive-lock-held ((slot-value connection 'condlock))
    (when (and (chanl:recv-blocks-p (slot-value connection 'request-queue))
               (chanl:recv-blocks-p (slot-value connection 'outbox)))
      (bt:condition-wait (slot-value connection 'condvar)
                         (slot-value connection 'condlock)))))

(defgeneric add-message-to-queue (connection message)
  ;; batch
  (:method ((connection connection) (messages list))
    (if (typep (first messages) 'request)
        (send-and-notify connection (connection-request-queue connection) messages)
        (dolist (response messages)
          (add-message-to-queue connection response)))
    (values))

  (:method ((connection connection) (message request))
    (send-and-notify connection (connection-request-queue connection) message)
    (values))

  (:method ((connection connection) (message response))
    (let ((id (response-id message)))
      (unless id
        (warn "Unexpected response which has no id. Ignored.")
        (return-from add-message-to-queue))

      (let ((response-map (connection-response-map connection))
            (response-lock (connection-response-lock connection))
            (response-callback (connection-response-callback connection)))
        (bt:with-recursive-lock-held (response-lock)
          (let ((callback (gethash id response-callback)))
            (if callback
                (progn
                  (handler-case
                      (funcall callback message)
                    (error (e)
                      (vom:error "~A in a JSON-RPC response callback: ~A"
                                 (type-of e)
                                 e)))
                  (remhash id response-callback))
                (setf (gethash id response-map) message))))))

    (values)))

(defun add-message-to-outbox (connection message)
  (send-and-notify connection (connection-outbox connection) message))

(defun set-callback-for-id (connection id callback)
  (let ((response-map (connection-response-map connection))
        (response-lock (connection-response-lock connection))
        (response-callback (connection-response-callback connection)))
    (bt:with-recursive-lock-held (response-lock)
      (multiple-value-bind (response existsp)
          (gethash id response-map)
        (if existsp
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
    (let ((*connection* connection)
          (bt:*default-special-bindings* (append `((*connection* . ,connection))
                                                 bt:*default-special-bindings*)))
      (funcall (connection-request-callback connection) request))))
