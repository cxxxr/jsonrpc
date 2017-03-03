(in-package #:cl-user)
(defpackage #:jsonrpc
  (:nicknames #:jsonrpc/main)
  (:use #:cl
        #:jsonrpc/request-response
        #:jsonrpc/transport/interface
        #:jsonrpc/class
        #:jsonrpc/errors)
  (:export
   ;; from request-response
   #:request
   #:response
   #:make-request
   #:make-response
   #:request-method
   #:request-params
   #:request-id
   #:response-error
   #:response-result
   #:response-id
   #:parse-message

   ;; from transports
   #:transport
   #:send-message
   #:receive-message

   ;; from class
   #:server-listen
   #:client-connect
   #:client-disconnect
   #:expose
   #:register-method
   #:clear-methods
   #:call-to
   #:call-async-to
   #:notify-to
   #:call
   #:call-async
   #:notify
   #:notify-async

   ;; from errors
   #:jsonrpc-error
   #:jsonrpc-parse-error
   #:jsonrpc-invalid-request
   #:jsonrpc-invalid-response
   #:jsonrpc-method-not-found
   #:jsonrpc-invalid-params
   #:jsonrpc-internal-error
   #:jsonrpc-server-error
   #:jsonrpc-error-code
   #:jsonrpc-error-message

   ;; from this package
   #:make-server
   #:make-client))
(in-package #:jsonrpc)

(defun make-client ()
  (make-instance 'client))

(defun make-server ()
  (make-instance 'server))
