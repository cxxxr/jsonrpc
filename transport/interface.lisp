(in-package #:cl-user)
(defpackage #:jsonrpc/transport/interface
  (:use #:cl)
  (:export #:transport
           #:transport-app
           #:transport-clients
           #:start-server
           #:start-client
           #:handle-request
           #:send-message-using-transport
           #:receive-message-using-transport
           #:send-message
           #:receive-message))
(in-package #:jsonrpc/transport/interface)

(defvar *transport*)

(defclass transport ()
  ((clients :initform nil
            :accessor transport-clients)
   (app :type function
        :initarg :app
        :accessor transport-app)))

(defgeneric start-server (transport))

(defgeneric start-client (transport))

(defgeneric handle-request (transport socket)
  (:method :around (transport socket)
    (let ((*transport* transport))
      (call-next-method))))

(defgeneric send-message-using-transport (transport message))

(defgeneric receive-message-using-transport (transport &optional socket))

(defun send-message (message &optional (transport *transport*))
  (assert transport)
  (send-message-using-transport transport message))

(defun receive-message (&optional (transport *transport*))
  (assert transport)
  (receive-message-using-transport transport))
