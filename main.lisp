(in-package #:cl-user)
(defpackage #:jsonrpc
  (:nicknames #:jsonrpc/main)
  (:use #:cl
        #:jsonrpc/request-response
        #:jsonrpc/transport/interface
        #:jsonrpc/class
        #:jsonrpc/errors
        #:jsonrpc/class)
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
   #:send-message
   #:receive-message

   ;; from class
   #:server-listen
   #:client-connect
   #:register-method

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
   #:notify
   #:make-server
   #:make-client))
(in-package #:jsonrpc)

(defun call (jsonrpc method &optional params)
  (check-type params (or list hash-table structure-object standard-object))
  (let ((id (make-id))
        (transport (jsonrpc-transport jsonrpc)))
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

(defun notify (jsonrpc method &optional params)
  (send-message
   (make-request :method method
                 :params params)
   (jsonrpc-transport jsonrpc)))

(defun make-client ()
  (make-instance 'client))

(defun make-server ()
  (make-instance 'server))
