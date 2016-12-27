(in-package #:cl-user)
(defpackage #:jsonrpc/transport/interface
  (:use #:cl)
  (:export #:transport
           #:transport-app
           #:transport-handle
           #:start-server
           #:start-client
           #:handle-request
           #:send-message-using-transport
           #:receive-message-using-transport
           #:send-message
           #:receive-message))
(in-package #:jsonrpc/transport/interface)

(defvar *transport*)
(defvar *handle*)

(defclass transport ()
  ((app :type function
        :initarg :app
        :accessor transport-app)
   (handle :initarg :handle
           :accessor transport-handle)))

(defgeneric start-server (transport))

(defgeneric start-client (transport))

(defgeneric handle-request (transport handle)
  (:method :around (transport handle)
    (let ((*transport* transport)
          (*handle* handle))
      (call-next-method))))

(defgeneric send-message-using-transport (to handle message))

(defgeneric receive-message-using-transport (from handle))

(defun send-message (message &optional to)
  (typecase to
    (transport
     (send-message-using-transport to (transport-handle to) message))
    (null
     (send-message-using-transport *transport* *handle* message))
    (otherwise
     (send-message-using-transport *transport* to message))))

(defun receive-message (&optional from)
  (typecase from
    (transport
     (receive-message-using-transport from (transport-handle from)))
    (null
     (receive-message-using-transport *transport* *handle*))
    (otherwise
     (receive-message-using-transport *transport* from))))
