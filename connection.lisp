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
                #:*default-special-bindings*)
  (:import-from #:dissect
                #:present)
  (:import-from #:event-emitter
                #:event-emitter)
  (:import-from #:chanl)
  (:import-from #:vom)
  (:export #:connection
           #:*connection*
           #:wait-for-ready
           #:connection-socket
           #:connection-request-callback
           #:add-message-to-queue
           #:add-message-to-outbox
           #:process-request
           #:connection-request-queue
           #:connection-outbox)
  (:documentation "jsonrpc/connection provides a class `connection' for holding data of each connections, like inbox and outbox."))
(in-package #:jsonrpc/connection)

(defvar *connection*)

(defclass process-wait ()
  ((condvar :initform (bt:make-condition-variable))
   (condlock :initform (bt:make-recursive-lock))))

(defgeneric wait-for-ready (process-wait)
  (:method ((process-wait process-wait))
    (bt:with-recursive-lock-held ((slot-value process-wait 'condlock))
      (bt:condition-wait (slot-value process-wait 'condvar)
                         (slot-value process-wait 'condlock)))))

(defgeneric notify-ready (process-wait)
  (:method ((process-wait process-wait))
    (bt:with-recursive-lock-held ((slot-value process-wait 'condlock))
      (bt:condition-notify (slot-value process-wait 'condvar)))))

(defclass connection (event-emitter process-wait)
  ((socket :initarg :socket
           :accessor connection-socket)
   (request-callback :initarg :request-callback
                     :accessor connection-request-callback)

   (request-queue :initform (make-instance 'chanl:unbounded-channel)
                  :accessor connection-request-queue)

   (response-map :initform (make-hash-table :test 'equal))
   (response-lock :initform (bt:make-lock))
   (response-callback :initform (make-hash-table :test 'equal))

   (outbox :initform (make-instance 'chanl:unbounded-channel)
           :accessor connection-outbox)))

(defgeneric add-message-to-queue (connection message)
  ;; batch
  (:method ((connection connection) (messages list))
    (if (typep (first messages) 'request)
        (progn
          (chanl:send (slot-value connection 'request-queue) messages)
          (notify-ready connection))
        (dolist (response messages)
          (add-message-to-queue connection response)))
    (values))

  (:method ((connection connection) (message request))
    (chanl:send (slot-value connection 'request-queue) message)
    (notify-ready connection)
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
  (chanl:send (connection-outbox connection) message)
  (notify-ready connection))

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
    (let ((*connection* connection)
          (bt:*default-special-bindings* (append `((*connection* . ,connection))
                                                 bt:*default-special-bindings*)))
      (funcall (connection-request-callback connection) request))))
