(in-package #:cl-user)
(defpackage #:jsonrpc/mapper
  (:use #:cl
        #:jsonrpc/errors)
  (:import-from #:dissect)
  (:import-from #:jsonrpc/request-response
                #:request
                #:request-method
                #:request-params
                #:make-response
                #:make-error-response
                #:request-id)
  (:import-from #:jsonrpc/errors
                #:jsonrpc-error
                #:jsonrpc-method-not-found
                #:jsonrpc-invalid-params)
  (:export #:exposable
           #:expose
           #:register-method
           #:clear-methods
           #:dispatch))
(in-package #:jsonrpc/mapper)

(defclass exposable ()
  ((mapper :initform (make-hash-table :test 'equal)
           :accessor exposable-mapper)))

(defgeneric expose (object method-name function)
  (:method ((object exposable) method-name function)
    (setf (gethash method-name (exposable-mapper object)) function)))
(setf (fdefinition 'register-method) #'expose)

(defgeneric clear-methods (object)
  (:method ((object exposable))
    (setf (exposable-mapper object) (make-hash-table :test 'equal))
    (values)))

(defgeneric dispatch (object message)
  (:method ((object exposable) (request request))
    (let ((handler (gethash (request-method request)
                            (exposable-mapper object))))
      (unless handler
        (error 'jsonrpc-method-not-found))
      (let ((result (handler-bind (#+ccl
                                   (ccl::wrong-number-of-arguments
                                     (lambda (e)
                                       (declare (ignore e))
                                       (error 'jsonrpc-invalid-params)))
                                   #+sbcl
                                   (sb-int:simple-program-error
                                     (lambda (e)
                                       (let ((message (simple-condition-format-control e)))
                                         (when (equal message "invalid number of arguments: ~S")
                                           (error 'jsonrpc-invalid-params))))))
                      (funcall handler (request-params request)))))
        (when (request-id request)
          (make-response :id (request-id request)
                         :result result)))))
  (:method :around ((object exposable) (request request))
    (handler-case
        (handler-bind ((error
                         (lambda (e)
                           (unless (typep e 'jsonrpc-error)
                             (cond
                               (*debug-on-error*
                                (invoke-debugger e))
                               (t
                                (dissect:present e)))))))
          (call-next-method))
      (jsonrpc-error (e)
        (when (request-id request)
          (make-error-response
           :id (request-id request)
           :code (jsonrpc-error-code e)
           :message (jsonrpc-error-message e))))
      (error ()
        (when (request-id request)
          (let ((e (make-condition 'jsonrpc-internal-error)))
            (make-error-response
             :id (request-id request)
             :code (jsonrpc-error-code e)
             :message (jsonrpc-error-message e))))))))
