(defpackage :jsonrpc/base
  (:use :cl)
  (:import-from #:jsonrpc/mapper
                #:exposable)
  (:import-from #:jsonrpc/transport/interface
                #:transport
                #:receive-message-using-transport)
  (:import-from #:jsonrpc/connection
                #:connection
                #:set-callback-for-id
                #:add-message-to-outbox)
  (:import-from #:jsonrpc/request-response
                #:make-request
                #:response-error
                #:response-error-code
                #:response-error-message
                #:response-result)
  (:import-from #:jsonrpc/errors
                #:jsonrpc-callback-error)
  (:import-from #:jsonrpc/utils
                #:hash-exists-p
                #:make-id)
  (:import-from #:chanl)
  (:import-from #:trivial-timeout
                #:with-timeout
                #:timeout-error)
  (:export #:jsonrpc
           #:jsonrpc-transport
           #:ensure-connected
           #:send-message
           #:receive-message
           #:call-async-to
           #:call-to
           #:*default-timeout*
           #:notify-to
           #:call
           #:call-async
           #:notify
           #:notify-async
           #:on-open-connection
           #:on-close-connection))
(in-package :jsonrpc/base)

(defgeneric call (jsonrpc method &optional params &rest options))
(defgeneric call-async (jsonrpc method &optional params callback error-callback))
(defgeneric notify (jsonrpc method &optional params))
(defgeneric notify-async (jsonrpc method &optional params))
(defgeneric on-open-connection (jsonrpc connection))
(defgeneric on-close-connection (jsonrpc connection))
(defgeneric call-to (from-client to-connection method &optional params &rest options)
  (:documentation "Makes a synchronouse RPC call. Should return an instance of JSONRPC/REQUEST-RESPONSE:RESPONSE class."))

(deftype jsonrpc-params () '(or list array hash-table structure-object standard-object condition))

(defclass jsonrpc (exposable)
  ((transport :type (or null transport)
              :initarg :transport
              :initform nil
              :accessor jsonrpc-transport)))

(defmethod ensure-connected ((jsonrpc jsonrpc))
  (unless (jsonrpc-transport jsonrpc)
    (error "Connection isn't established yet for ~A" jsonrpc)))

(defgeneric send-message (to connection message)
  (:method (to connection message)
    (declare (ignore to))
    (add-message-to-outbox connection message)))

(defun receive-message (from connection)
  (ensure-connected from)
  (receive-message-using-transport (jsonrpc-transport from) connection))

(defun call-async-to (from to method &optional params callback error-callback)
  (check-type params jsonrpc-params)
  (let ((id (make-id)))
    (set-callback-for-id to
                         id
                         (lambda (response)
                           (if (response-error response)
                               (and error-callback
                                    (funcall error-callback
                                             (response-error-message response)
                                             (response-error-code response)))
                               (and callback
                                    (funcall callback (response-result response))))))

    (let ((request (make-request :id id
                                 :method method
                                 :params params)))
      (send-message from
                    to
                    request)
      request)))

(defvar *call-to-result* (make-hash-table :test 'eq))
(defvar *call-to-error* (make-hash-table :test 'eq))
(defvar *default-timeout* 60)

(defmethod call-to (from to method &optional params &rest options)
  (destructuring-bind (&key (timeout *default-timeout*)) options
    (let ((channel (make-instance 'chanl:unbounded-channel)))
      (call-async-to from to
                     method
                     params
                     (lambda (res)
                       (chanl:send channel res))
                     (lambda (message code)
                       (chanl:send channel (make-condition 'jsonrpc-callback-error
                                                           :message message
                                                           :code code))))
      (let ((result (handler-case (with-timeout (timeout)
                                    (chanl:recv channel))
                      (timeout-error (e)
                        (declare (ignore e))
                        (error 'jsonrpc-timeout
                               :message "JSON-RPC synchronous call has been timeout")))))
        (if (typep result 'error)
            (error result)
            result)))))

(defun notify-to (from to method &optional params)
  (check-type params jsonrpc-params)
  (send-message from
                to
                (make-request :method method
                              :params params)))
