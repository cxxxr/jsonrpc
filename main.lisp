(in-package #:cl-user)
(defpackage #:jsonrpc
  (:nicknames #:jsonrpc/main)
  (:use #:jsonrpc/request-response
        #:jsonrpc/transports
        #:jsonrpc/server
        #:jsonrpc/errors)
  (:shadowing-import-from #:jsonrpc/server
                          #:start-server)
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
   #:parse-request
   #:parse-response

   ;; from transports
   #:transport
   #:tcp-transport
   #:send-message

   ;; from server
   #:start-server

   ;; from errors
   #:jsonrpc-error
   #:jsonrpc-parse-error
   #:invalid-request
   #:invalid-response
   #:method-not-found
   #:invalid-params
   #:internal-error
   #:server-error
   #:jsonrpc-error-code
   #:jsonrpc-error-message))
