(in-package #:cl-user)
(defpackage #:jsonrpc/transport/interface
  (:use #:cl)
  (:export #:transport
           #:transport-app
           #:transport-connection
           #:start-server
           #:start-client
           #:handle-request
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
   (connection :accessor transport-connection)))

(defgeneric start-server (transport))

(defgeneric start-client (transport))

(defgeneric handle-request (transport connection)
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
