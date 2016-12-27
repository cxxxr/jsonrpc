(in-package #:cl-user)
(defpackage #:jsonrpc
  (:nicknames #:jsonrpc/main)
  (:use #:cl
        #:jsonrpc/request-response
        #:jsonrpc/transports
        #:jsonrpc/server
        #:jsonrpc/client
        #:jsonrpc/errors)
  (:import-from #:jsonrpc/utils
                #:make-id)
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
   #:tcp-transport
   #:send-message
   #:receive-message

   ;; from server
   #:server-listen

   ;; from client
   #:client-connect

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
   #:jsonrpc-error-message

   ;; from this package
   #:call))
(in-package #:jsonrpc)

(defun call (transport method &rest params)
  (let ((id (make-id)))
    (send-message
     (make-request :id id
                   :method method
                   :params params)
     transport)
    (let ((response (receive-message transport)))
      (when (response-error response)
        (error "JSON-RPC response error: ~A" (response-error response)))
      (unless (equal (response-id response) id)
        (error "Unmatched response id"))
      (response-result response))))
