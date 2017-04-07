(in-package #:cl-user)
(defpackage #:jsonrpc/errors
  (:use #:cl)
  (:import-from #:yason)
  (:export #:jsonrpc-error
           #:jsonrpc-parse-error
           #:jsonrpc-invalid-request
           #:jsonrpc-invalid-response
           #:jsonrpc-method-not-found
           #:jsonrpc-invalid-params
           #:jsonrpc-internal-error
           #:jsonrpc-server-error
           #:jsonrpc-callback-error
           #:jsonrpc-error-code
           #:jsonrpc-error-message))
(in-package #:jsonrpc/errors)

(define-condition jsonrpc-error (error)
  ((code :initarg :code
         :initform -1
         :accessor jsonrpc-error-code)
   (message :initarg :message
            :initform ""
            :accessor jsonrpc-error-message)))

(define-condition jsonrpc-parse-error (jsonrpc-error)
  ((code :initform -32700)
   (message :initform "Parse error")))

(define-condition jsonrpc-invalid-request (jsonrpc-error)
  ((code :initform -32600)
   (message :initform "Invalid Request")))

(define-condition jsonrpc-invalid-response (jsonrpc-error)
  ((code :initform -32000)
   (message :initform "Invalid Response")))

(define-condition jsonrpc-method-not-found (jsonrpc-error)
  ((code :initform -32601)
   (message :initform "Method not found")))

(define-condition jsonrpc-invalid-params (jsonrpc-error)
  ((code :initform -32602)
   (message :initform "Invalid params")))

(define-condition jsonrpc-internal-error (jsonrpc-error)
  ((code :initform -32603)
   (message :initform "Internal error")))

(define-condition jsonrpc-server-error (jsonrpc-error) ())

(define-condition jsonrpc-callback-error (jsonrpc-error) ())

(defmethod yason:encode ((object jsonrpc-error) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "code" (jsonrpc-error-code object))
      (yason:encode-object-element "message" (jsonrpc-error-message object)))))
