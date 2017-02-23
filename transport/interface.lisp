(in-package #:cl-user)
(defpackage #:jsonrpc/transport/interface
  (:use #:cl
        #:jsonrpc/errors)
  (:import-from #:jsonrpc/request-response
                #:request-id
                #:make-error-response)
  (:import-from #:dissect)
  (:export #:*connection*
           #:transport
           #:transport-message-callback
           #:transport-connection
           #:start-server
           #:start-client
           #:handle-request-message
           #:handle-request
           #:process-message
           #:send-message
           #:receive-message
           #:send-message
           #:receive-message))
(in-package #:jsonrpc/transport/interface)

(defvar *transport*)
(defvar *connection*)

(defclass transport ()
  ((message-callback :initarg :message-callback
                     :accessor transport-message-callback)
   (connection :accessor transport-connection)))

(defgeneric start-server (transport))

(defgeneric start-client (transport)
  (:method (transport)
    transport))

(defgeneric process-message (transport message)
  (:method (transport message)
    (if (listp message)
        (remove-if #'null
                   (mapcar (lambda (message)
                             (process-message transport message))
                           message))
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
               :message (jsonrpc-error-message e))))))))

(defgeneric handle-request-message (transport connection message)
  (:method (transport connection message)
    (let ((response (process-message transport message)))
      (when response
        (send-message transport connection response))))
  (:method :around (transport connection message)
    (declare (ignore message))
    (let ((*transport* transport)
          (*connection* connection))
      (call-next-method))))

(defun handle-request (transport connection)
  (let ((message (receive-message transport connection)))
    (when message
      (handle-request-message transport connection message))))

(defgeneric send-message (transport to message))

(defgeneric receive-message (transport from))
