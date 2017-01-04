(in-package #:cl-user)
(defpackage #:jsonrpc/transport/interface
  (:use #:cl
        #:jsonrpc/errors)
  (:import-from #:jsonrpc/request-response
                #:request-id
                #:make-error-response)
  (:export #:transport
           #:transport-app
           #:transport-connection
           #:transport-data
           #:start-server
           #:start-client
           #:handle-request
           #:process-message
           #:send-message-using-transport
           #:receive-message-using-transport
           #:send-message
           #:receive-message))
(in-package #:jsonrpc/transport/interface)

(defvar *transport*)
(defvar *connection*)

(defclass transport ()
  ((app :type function
        :initarg :app
        :accessor transport-app)
   (connection :accessor transport-connection)
   (data :initform nil
         :accessor transport-data)))

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
        (handler-case (funcall (transport-app transport) message)
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

(defgeneric handle-request (transport connection)
  (:method (transport connection)
    (let ((message (receive-message-using-transport transport connection)))
      (when message
        (let ((response (process-message transport message)))
          (when response
            (send-message-using-transport transport connection response))))))
  (:method :around (transport connection)
    (let ((*transport* transport)
          (*connection* connection))
      (call-next-method))))

(defgeneric send-message-using-transport (to connection message))

(defgeneric receive-message-using-transport (from connection))

(defun send-message (message &optional to)
  (typecase to
    (transport
     (send-message-using-transport to (transport-connection to) message))
    (null
     (send-message-using-transport *transport* *connection* message))
    (otherwise
     (send-message-using-transport *transport* to message))))

(defun receive-message (&optional from)
  (typecase from
    (transport
     (receive-message-using-transport from (transport-connection from)))
    (null
     (receive-message-using-transport *transport* *connection*))
    (otherwise
     (receive-message-using-transport *transport* from))))
