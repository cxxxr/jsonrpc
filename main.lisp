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
  (:import-from #:jsonrpc/server/mapper
                #:make-mapper
                #:register-method)
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

   ;; from server/mapper
   #:make-mapper
   #:register-method

   ;; from client
   #:client-connect

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
   #:call
   #:notify))
(in-package #:jsonrpc)

(defun call (transport method &optional params)
  (let ((id (make-id)))
    (send-message
     (make-request :id id
                   :method method
                   :params params)
     transport)
    (let ((response (receive-message transport)))
      (when (response-error response)
        (error "JSON-RPC response error: ~A (Code: ~A)"
               (response-error-message response)
               (response-error-code response)))
      (unless (equal (response-id response) id)
        (error "Unmatched response id"))
      (response-result response))))

(defun notify (transport method &optional params)
  (send-message
   (make-request :method method
                 :params params)
   transport))
