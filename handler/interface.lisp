(in-package #:cl-user)
(defpackage #:jsonrpc/handler/interface
  (:use #:cl)
  (:export #:handler
           #:handler-app
           #:handler-clients
           #:start-handler
           #:handle-request
           #:send-message-using-handler
           #:send-message
           #:handler-clients))
(in-package #:jsonrpc/handler/interface)

(defvar *handler*)
(defvar *socket*)

(defclass handler ()
  ((clients :initform nil
            :accessor handler-clients)
   (app :type function
        :initarg :app
        :accessor handler-app)))

(defgeneric start-handler (handler))

(defgeneric handle-request (handler socket)
  (:method :around (handler socket)
    (let ((*handler* handler)
          (*socket* socket))
      (call-next-method))))

(defgeneric send-message-using-handler (handler socket message))

(defun send-message (message)
  (send-message-using-handler *handler* *socket* message))
