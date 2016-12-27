(in-package #:cl-user)
(defpackage #:jsonrpc/errors
  (:use #:cl)
  (:export #:jsonrpc-error
           #:jsonrpc-parse-error
           #:jsonrpc-invalid-request
           #:invalid-response
           #:method-not-found
           #:invalid-params
           #:internal-error
           #:server-error
           #:jsonrpc-error-code
           #:jsonrpc-error-message))
(in-package #:jsonrpc/errors)

(define-condition jsonrpc-error (error)
  ((code :accessor jsonrpc-error-code)
   (message :accessor jsonrpc-error-message
            :initarg :message)))

(define-condition jsonrpc-parse-error (jsonrpc-error)
  ((code :initform -32700)
   (message :initform "Parse error")))

(define-condition jsonrpc-invalid-request (jsonrpc-error)
  ((code :initform -32600)
   (message :initform "Invalid Request")))

(define-condition jsonrpc-invalid-response (jsonrpc-error) ())

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
