(defpackage #:jsonrpc
  (:nicknames #:jsonrpc/main)
  (:use #:cl
        #:jsonrpc/request-response
        #:jsonrpc/transport/interface
        #:jsonrpc/base
        #:jsonrpc/client
        #:jsonrpc/server
        #:jsonrpc/errors
        #:jsonrpc/mapper)
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

   ;; from base/server/client
   #:*default-timeout*
   #:*jsonrpc-version*
   #:server
   #:client
   #:send-message
   #:receive-message
   #:server-listen
   #:client-connect
   #:client-disconnect
   #:expose
   #:register-method
   #:clear-methods
   #:dispatch
   #:call-to
   #:call-async-to
   #:notify-to
   #:call
   #:call-async
   #:notify
   #:notify-async
   #:broadcast
   #:multicall-async

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
   #:*debug-on-error*

   ;; from this package
   #:make-server
   #:make-client))
(in-package #:jsonrpc)

(declaim (ftype (function (&key (:version jsonrpc-version)))))
(defun make-client (&key (version *jsonrpc-version*))
  "Creates and returns a new instance of the client class.
Optionally the jsonrpc version can be supplied. Valid values are 2.0 or 1.0"
  (make-instance 'client :version version))

(defun make-server ()
  (make-instance 'server))
