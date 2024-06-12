(defpackage #:jsonrpc/server
  (:use #:cl)
  (:use #:jsonrpc/base)
  (:import-from #:jsonrpc/base
                #:jsonrpc
                #:jsonrpc-transport)
  (:import-from #:jsonrpc/mapper
                #:dispatch)
  (:import-from #:jsonrpc/transport/interface
                #:transport-message-callback
                #:transport
                #:start-server)
  (:import-from #:jsonrpc/connection
                #:connection
                #:*connection*)
  (:import-from #:jsonrpc/request-response
                #:make-request)
  (:import-from #:jsonrpc/utils
                #:find-mode-class)
  (:import-from #:alexandria
                #:deletef
                #:remove-from-plist)
  (:export
   :server
   :on-adding-connection
   :on-removing-connection
   :on-close-server-connection
   :on-open-server-transport
   :bind-server-to-transport
   :server-listen
   :broadcast
   :multicall-async))
(in-package #:jsonrpc/server)

(defclass server (jsonrpc)
  ((client-connections :initform '()
                       :accessor server-client-connections)
   (%lock :initform (bt:make-lock "client-connections-lock")
          :reader server-lock)))


(defmethod on-adding-connection (server connection)
  (values))

(defmethod on-removing-connection (server connection)
  (values))

(defmethod on-close-server-connection ((server server) connection)
  (bt:with-lock-held ((server-lock server))
    (on-removing-connection server connection)
    (deletef (server-client-connections server) connection)))

(defmethod on-open-server-transport ((server server) connection)
  (bt:with-lock-held ((server-lock server))
    (on-adding-connection server connection)
    (push connection (server-client-connections server))))

(defun bind-server-to-transport (server transport)
  "Initializes all necessary event handlers inside TRANSPORT to process calls to the SERVER.

   This function can be usefule if you want to create server and transport instance manually,
   and then to start transport as part of a bigger server."
  (setf (jsonrpc-transport server) transport)

  (setf (transport-message-callback transport)
        (lambda (message)
          (dispatch server message))))


(defun server-listen (server &rest initargs &key mode &allow-other-keys)
  (let* ((class (find-mode-class mode))
         (initargs (remove-from-plist initargs :mode))
         (bt:*default-special-bindings* `((*standard-output* . ,*standard-output*)
                                          (*error-output* . ,*error-output*)
                                          ,@bt:*default-special-bindings*)))
    (unless class
      (error "Unknown mode ~A" mode))
    (let ((transport (apply #'make-instance class
                            :jsonrpc server
                            initargs)))
      (bind-server-to-transport server transport)
      (start-server transport)))
  server)

(defmethod call-async ((server server) method &optional params callback error-callback)
  (unless (boundp '*connection*)
    (error "`call' is called outside of handlers."))
  (call-async-to server *connection* method params callback error-callback))

(defmethod notify ((server server) method &optional params)
  (unless (boundp '*connection*)
    (error "`notify' is called outside of handlers."))
  (notify-to server *connection*
             method params))

(defmethod notify-async ((server server) method &optional params)
  (unless (boundp '*connection*)
    (error "`notify-async' is called outside of handlers."))
  (send-message server *connection*
                (make-request :method method
                              :params params)))

;; Experimental
(defmethod broadcast ((server server) method &optional params)
  (dolist (conn (server-client-connections server))
    (notify-to server conn method params)))

;; Experimental
(defmethod multicall-async ((server server) method &optional params callback error-callback)
  (dolist (conn (server-client-connections server))
    (call-async-to server conn method params
                   callback
                   error-callback)))
