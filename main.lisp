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
   #:*connection*
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
  (let ((id (make-id)))
    (send-message jsonrpc
                  *connection*
                  (make-request :id id
                                :method method
                                :params params))
    (let ((response (receive-message jsonrpc *connection*)))
      (when (response-error response)
        (error "JSON-RPC response error: ~A (Code: ~A)"
               (response-error-message response)
               (response-error-code response)))
      (unless (equal (response-id response) id)
        (error "Unmatched response id"))
      (response-result response))))

(defun notify (jsonrpc method &optional params)
  (send-message jsonrpc
                *connection*
                (make-request :method method
                              :params params)))

(defun make-client ()
  (make-instance 'client))

(defun make-server ()
  (make-instance 'server))
