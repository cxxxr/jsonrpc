(in-package #:cl-user)
(defpackage #:jsonrpc/transport/interface
  (:use #:cl
        #:jsonrpc/errors)
  (:import-from #:jsonrpc/request-response
                #:request
                #:response
                #:request-id
                #:response-id
                #:make-error-response)
  (:import-from #:bordeaux-threads)
  (:import-from #:dissect)
  (:export #:*connection*
           #:transport
           #:transport-message-callback
           #:transport-connection
           #:set-callback-for-id
           #:start-server
           #:start-client
           #:handle-message
           #:process-message
           #:send-message-using-transport
           #:receive-message-using-transport))
(in-package #:jsonrpc/transport/interface)

(defvar *connection*)

(defclass transport ()
  ((message-callback :initarg :message-callback
                     :accessor transport-message-callback)
   (connection :accessor transport-connection)

   ;; Inbox
   (inbox :initform (make-hash-table :test 'equal)
          :reader transport-inbox)
   (inbox-callback :initform (make-hash-table :test 'equal)
                   :reader transport-inbox-callback)
   (inbox-lock :initform (bt:make-lock))))

(defun push-response (transport message)
  (check-type message response)
  (let ((id (response-id message)))
    (with-slots (inbox inbox-callback inbox-lock) transport
      (bt:with-lock-held (inbox-lock)
        (let ((callback (gethash id inbox-callback)))
          (if callback
              (progn
                (funcall callback message)
                (remhash id inbox-callback))
              (setf (gethash id inbox) message))))))
  (values))

(defun set-callback-for-id (transport id callback)
  (with-slots (inbox inbox-callback inbox-lock) transport
    (bt:with-lock-held (inbox-lock)
      (let ((response (gethash id inbox)))
        (if response
            ;; The response is ready
            (progn
              (funcall callback response)
              (remhash id inbox))
            (setf (gethash id inbox-callback)
                  callback)))))
  (values))

(defgeneric start-server (transport))

(defgeneric start-client (transport)
  (:method (transport)
    transport))

(defgeneric process-message (transport message)
  (:method (transport (message list))
    (remove-if #'null
               (mapcar (lambda (message)
                         (process-message transport message))
                       message)))
  (:method (transport (message request))
    (handler-case
        (handler-bind ((error
                         (lambda (e)
                           (unless (typep e 'jsonrpc-error)
                             (dissect:present e)))))
          (funcall (transport-message-callback transport) message))
      (jsonrpc-error (e)
        (make-error-response
         :id (request-id message)
         :code (jsonrpc-error-code e)
         :message (jsonrpc-error-message e)))
      (error ()
        (let ((e (make-condition 'jsonrpc-internal-error)))
          (make-error-response
           :id (request-id message)
           :code (jsonrpc-error-code e)
           :message (jsonrpc-error-message e))))))
  (:method (transport (message response))
    (push-response transport message)))

(defgeneric handle-message (transport connection message)
  (:method (transport connection message)
    (let ((response (process-message transport message)))
      (when response
        (send-message-using-transport transport connection response))))
  (:method :around (transport connection message)
    (declare (ignore message))
    (let ((*connection* connection))
      (call-next-method))))

(defgeneric send-message-using-transport (transport to message))

(defgeneric receive-message-using-transport (transport from))
