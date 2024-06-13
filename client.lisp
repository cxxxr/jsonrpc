(defpackage #:jsonrpc/client
  (:use #:cl)
  (:import-from #:jsonrpc/base
                #:jsonrpc
                #:jsonrpc-transport
                #:ensure-connected
                #:send-message
                #:call-async-to
                #:call-to
                #:notify-to
                #:call
                #:call-async
                #:notify
                #:notify-async)
  (:import-from #:jsonrpc/mapper
                #:dispatch)
  (:import-from #:jsonrpc/transport/interface
                #:transport
                #:transport-connection
                #:transport-threads
                #:start-client)
  (:import-from #:jsonrpc/connection
                #:connection)
  (:import-from #:jsonrpc/request-response
                #:make-request
                #:jsonrpc-version
                #:*jsonrpc-version*)
  (:import-from #:jsonrpc/utils
                #:find-mode-class)
  (:import-from #:alexandria
                #:remove-from-plist)
  (:export #:client
           #:client-connect-using-class
           #:client-connect
           #:client-disconnect))
(in-package #:jsonrpc/client)

(defclass client (jsonrpc)
  ((version :type jsonrpc-version
            :initform *jsonrpc-version*
            :initarg :version
            :accessor version
            :documentation "JSON-RPC version of the client. Default is *jsonrpc-version* which is 2.0, while support for 1.0 is experimental."))
  (:documentation "A client is used for creating requests."))

(defmethod jsonrpc/base:on-open-connection ((client client) connection)
  (declare (ignore connection)))

(defun client-connect-using-class (client class &rest initargs)
  (let* ((initargs (remove-from-plist initargs :mode))
         (bt:*default-special-bindings* `((*standard-output* . ,*standard-output*)
                                          (*error-output* . ,*error-output*)
                                          ,@bt:*default-special-bindings*)))
    (let ((transport (apply #'make-instance class
                            :jsonrpc client
                            :message-callback
                            (lambda (message)
                              (dispatch client message))
                            initargs)))
      (setf (jsonrpc-transport client) transport)
      (start-client transport)))
  client)

(defun client-connect (client &rest initargs &key mode &allow-other-keys)
  (let ((class (find-mode-class mode)))
    (unless class
      (error "Unknown mode ~A" mode))
    (apply #'client-connect-using-class client class initargs)))

(defun client-disconnect (client)
  (ensure-connected client)
  (let ((transport (jsonrpc-transport client)))
    (mapc #'bt2:destroy-thread (transport-threads transport))
    (setf (transport-threads transport) '())
    (setf (transport-connection transport) nil))
  (values))

(defmethod call ((client client) method &optional params &rest options)
  (ensure-connected client)
  (apply #'call-to client (transport-connection (jsonrpc-transport client))
         method params options))

(defmethod call-async ((client client) method &optional params callback error-callback)
  (ensure-connected client)
  (call-async-to client (transport-connection (jsonrpc-transport client))
                 method params
                 callback
                 error-callback))

(defmethod notify ((client client) method &optional params)
  (ensure-connected client)
  (notify-to client (transport-connection (jsonrpc-transport client))
             method params))

(defmethod notify-async ((client client) method &optional params)
  (ensure-connected client)
  (let ((connection (transport-connection (jsonrpc-transport client))))
    (send-message client connection
                  (make-request :method method
                                :params params))))
