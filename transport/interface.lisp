(in-package #:cl-user)
(defpackage #:jsonrpc/transport/interface
  (:use #:cl
        #:jsonrpc/errors)
  (:import-from #:jsonrpc/request-response
                #:request
                #:response
                #:request-id
                #:response-id
                #:make-error-response)
  (:import-from #:bordeaux-threads)
  (:import-from #:dissect)
  (:export #:*connection*
           #:transport
           #:transport-message-callback
           #:transport-connection
           #:wait-for-response
           #:start-server
           #:start-client
           #:handle-message
           #:process-message
           #:send-message-using-transport
           #:receive-message-using-transport))
(in-package #:jsonrpc/transport/interface)

(defvar *connection*)

(defclass transport ()
  ((message-callback :initarg :message-callback
                     :accessor transport-message-callback)
   (connection :accessor transport-connection)

   ;; Inbox
   (inbox :initform (make-hash-table :test 'equal)
          :reader transport-inbox)
   (condvar :initform (bt:make-condition-variable))
   (inbox-lock :initform (bt:make-lock))))

(defun push-response (transport message)
  (check-type message response)
  (with-slots (inbox-lock condvar) transport
    (bt:with-lock-held (inbox-lock)
      (setf (gethash (response-id message) (transport-inbox transport))
            message))
    (bt:condition-notify condvar)))

(defun wait-for-response (transport id)
  (flet ((find-response ()
           (gethash id (transport-inbox transport))))
    (or (find-response)
        (loop
          (with-slots (condvar inbox-lock) transport
            (bt:with-lock-held (inbox-lock)
              (bt:condition-wait condvar inbox-lock)
              (let ((response (find-response)))
                (when response
                  (remhash id (transport-inbox transport))
                  (return response)))))))))

(defgeneric start-server (transport))

(defgeneric start-client (transport)
  (:method (transport)
    transport))

(defgeneric process-message (transport message)
  (:method (transport (message list))
    (remove-if #'null
               (mapcar (lambda (message)
                         (process-message transport message))
                       message)))
  (:method (transport (message request))
    (handler-case
        (handler-bind ((error
                         (lambda (e)
                           (unless (typep e 'jsonrpc-error)
                             (dissect:present e)))))
          (funcall (transport-message-callback transport) message))
      (jsonrpc-error (e)
        (make-error-response
         :id (request-id message)
         :code (jsonrpc-error-code e)
         :message (jsonrpc-error-message e)))
      (error ()
        (let ((e (make-condition 'jsonrpc-internal-error)))
          (make-error-response
           :id (request-id message)
           :code (jsonrpc-error-code e)
           :message (jsonrpc-error-message e))))))
  (:method (transport (message response))
    (push-response transport message)))

(defgeneric handle-message (transport connection message)
  (:method (transport connection message)
    (let ((response (process-message transport message)))
      (when response
        (send-message-using-transport transport connection response))))
  (:method :around (transport connection message)
    (declare (ignore message))
    (let ((*connection* connection))
      (call-next-method))))

(defgeneric send-message-using-transport (transport to message))

(defgeneric receive-message-using-transport (transport from))
