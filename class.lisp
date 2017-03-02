(in-package #:cl-user)
(defpackage #:jsonrpc/class
  (:use #:cl)
  (:import-from #:jsonrpc/mapper
                #:make-mapper
                #:to-app
                #:register-method-to-mapper)
  (:import-from #:jsonrpc/transport/interface
                #:transport
                #:transport-connection
                #:start-server
                #:start-client
                #:send-message-using-transport
                #:receive-message-using-transport)
  (:import-from #:jsonrpc/connection
                #:*connection*
                #:set-callback-for-id)
  (:import-from #:jsonrpc/request-response
                #:make-request
                #:response-id
                #:response-error
                #:response-error-code
                #:response-error-message
                #:response-result)
  (:import-from #:jsonrpc/utils
                #:find-mode-class
                #:make-id)
  (:import-from #:bordeaux-threads
                #:*default-special-bindings*)
  (:import-from #:event-emitter
                #:on
                #:emit)
  (:import-from #:alexandria
                #:remove-from-plist)
  (:export #:client
           #:server
           #:jsonrpc-transport
           #:expose
           #:register-method
           #:clear-methods
           #:server-listen
           #:client-connect
           #:send-message
           #:receive-message
           #:call-to
           #:call-async-to
           #:notify-to
           #:call
           #:call-async
           #:notify))
(in-package #:jsonrpc/class)

(defclass jsonrpc ()
  ((mapper :initform (make-mapper)
           :accessor jsonrpc-mapper)
   (transport :type (or null transport)
              :initarg :transport
              :initform nil
              :accessor jsonrpc-transport)))

(defclass client (jsonrpc) ())

(defclass server (jsonrpc) ())

(defgeneric expose (object method-name function)
  (:method ((object jsonrpc) method-name function)
    (register-method-to-mapper (jsonrpc-mapper object)
                               method-name function)))
(setf (fdefinition 'register-method) #'expose)

(defun clear-methods (object)
  (setf (jsonrpc-mapper object) (make-mapper))
  object)

(defgeneric server-listen (server &rest initargs &key mode &allow-other-keys)
  (:method ((server server) &rest initargs &key (mode :tcp) &allow-other-keys)
    (let* ((class (find-mode-class mode))
           (initargs (remove-from-plist initargs :mode))
           (bt:*default-special-bindings* `((*standard-output* . ,*standard-output*)
                                            (*error-output* . ,*error-output*)) ))
      (unless class
        (error "Unknown mode ~A" mode))
      (let ((transport (apply #'make-instance class
                              :message-callback 
                              (to-app (jsonrpc-mapper server))
                              initargs)))
        (setf (jsonrpc-transport server) transport)

        (on :open transport
            (lambda (connection)
              (emit :open server connection)))

        (start-server transport)))
    server))

(defgeneric client-connect (client &rest initargs &key mode &allow-other-keys)
  (:method ((client client) &rest initargs &key (mode :tcp) &allow-other-keys)
    (let* ((class (find-mode-class mode))
           (initargs (remove-from-plist initargs :mode))
           (bt:*default-special-bindings* `((*standard-output* . ,*standard-output*)
                                            (*error-output* . ,*error-output*)) ))
      (unless class
        (error "Unknown mode ~A" mode))
      (let ((transport (apply #'make-instance class
                              :message-callback 
                              (to-app (jsonrpc-mapper client))
                              initargs)))
        (setf (jsonrpc-transport client) transport)

        (on :open transport
            (lambda (connection)
              (emit :open client connection)))

        (start-client transport)))
    client))

(defgeneric send-message (to connection message)
  (:method (to connection message)
    (send-message-using-transport (jsonrpc-transport to) connection message)))

(defgeneric receive-message (from connection)
  (:method (from connection)
    (receive-message-using-transport (jsonrpc-transport from) connection)))

(deftype jsonrpc-params () '(or list array hash-table structure-object standard-object))

(defun call-async-to (from to method &optional params (callback #'identity))
  (check-type params jsonrpc-params)
  (let ((id (make-id)))
    (set-callback-for-id to
                         id
                         (lambda (response)
                           (when (response-error response)
                             (error "JSON-RPC response error: ~A (Code: ~A)"
                                    (response-error-message response)
                                    (response-error-code response)))
                           (unless (equal (response-id response) id)
                             (error "Unmatched response id"))
                           (funcall callback (response-result response))))

    (send-message from
                  to
                  (make-request :id id
                                :method method
                                :params params))

    (values)))

(defun call-to (from to method &optional params)
  (let ((condvar (bt:make-condition-variable))
        (condlock (bt:make-lock))
        result)
    (call-async-to from to
                   method
                   params
                   (lambda (res)
                     (setf result res)
                     (bt:with-lock-held (condlock)
                       (bt:condition-notify condvar))))

    (bt:with-lock-held (condlock)
      (bt:condition-wait condvar condlock))

    result))

(defun notify-to (from to method &optional params)
  (check-type params jsonrpc-params)
  (send-message from
                to
                (make-request :method method
                              :params params)))

(defgeneric call (jsonrpc method &optional params)
  (:method ((client client) method &optional params)
    (call-to client (transport-connection (jsonrpc-transport client))
             method params)))

(defgeneric call-async (jsonrpc method &optional params callback)
  (:method ((client client) method &optional params callback)
    (call-async-to client (transport-connection (jsonrpc-transport client))
                   method params
                   callback))
  (:method ((server server) method &optional params callback)
    (unless (boundp '*connection*)
      (error "`call' is called outside of handlers."))
    (call-async-to server *connection* method params callback)))

(defgeneric notify (client method &optional params)
  (:method ((client client) method &optional params)
    (notify-to client (transport-connection (jsonrpc-transport client))
               method params))
  (:method ((server server) method &optional params)
    (unless (boundp '*connection*)
      (error "`notify' is called outside of handlers."))
    (notify-to server *connection*
               method params)))
