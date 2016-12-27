(in-package #:cl-user)
(defpackage #:jsonrpc/transport/interface
  (:use #:cl)
  (:export #:transport
           #:transport-app
           #:transport-clients
           #:start-server
           #:handle-request
           #:send-message-using-transport
           #:send-message))
(in-package #:jsonrpc/transport/interface)

(defvar *transport*)
(defvar *socket*)

(defclass transport ()
  ((clients :initform nil
            :accessor transport-clients)
   (app :type function
        :initarg :app
        :accessor transport-app)))

(defgeneric start-server (transport))

(defgeneric handle-request (transport socket)
  (:method :around (transport socket)
    (let ((*transport* transport)
          (*socket* socket))
      (call-next-method))))

(defgeneric send-message-using-transport (transport socket message))

(defun send-message (message)
  (send-message-using-transport *transport* *socket* message))
