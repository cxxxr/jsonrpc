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

    (send-message from
                  to
                  (make-request :id id
                                :method method
                                :params params))

    (values)))

(defvar *call-to-result* (make-hash-table :test 'eq))
(defvar *call-to-error* (make-hash-table :test 'eq))
(defvar *default-timeout* 60)

(defmethod call-to ((from jsonrpc) (to connection) (method string) &optional params &rest options)
  (destructuring-bind (&key (timeout *default-timeout*)) options
    (let ((condvar (bt:make-condition-variable))
          (condlock (bt:make-lock))
          (readylock (bt:make-lock)))
      (bt:acquire-lock readylock)
      (call-async-to from to
                     method
                     params
                     (lambda (res)
                       (bt:with-lock-held (readylock)
                         (bt:with-lock-held (condlock)
                           (setf (gethash readylock *call-to-result*) res)
                           (bt:condition-notify condvar))))
                     (lambda (message code)
                       (bt:with-lock-held (readylock)
                         (bt:with-lock-held (condlock)
                           (setf (gethash readylock *call-to-error*)
                                 (make-condition 'jsonrpc-callback-error
                                                 :message message
                                                 :code code))
                           (bt:condition-notify condvar)))))
      (bt:with-lock-held (condlock)
        (bt:release-lock readylock)
        (unless (bt:condition-wait condvar condlock :timeout timeout)
          (error "JSON-RPC synchronous call has been timeout")))

      ;; XXX: Strangely enough, there's sometimes no results/errors here on SBCL.
      #+(and sbcl linux)
      (loop repeat 5
            until (or (hash-exists-p *call-to-result* readylock)
                      (hash-exists-p *call-to-error* readylock))
            do (sleep 0.1))

      (multiple-value-bind (error error-exists-p)
          (gethash readylock *call-to-error*)
        (multiple-value-bind (result result-exists-p)
            (gethash readylock *call-to-result*)
          (assert (or error-exists-p
                      result-exists-p))
          (remhash readylock *call-to-error*)
          (remhash readylock *call-to-result*)
          (if error
              (error error)
              result))))))

(defun notify-to (from to method &optional params)
  (check-type params jsonrpc-params)
  (send-message from
                to
                (make-request :method method
                              :params params)))
