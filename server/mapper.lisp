(in-package #:cl-user)
(defpackage #:jsonrpc/server/mapper
  (:use #:cl)
  (:import-from #:jsonrpc/request-response
                #:request-method
                #:request-params
                #:make-response
                #:request-id)
  (:import-from #:jsonrpc/errors
                #:jsonrpc-method-not-found)
  (:export #:make-mapper
           #:register-method
           #:to-app))
(in-package #:jsonrpc/server/mapper)

(defun make-mapper ()
  (make-hash-table :test 'equal))

(defun register-method (mapper method-name function)
  (setf (gethash method-name mapper) function))

(defun find-handler (mapper method-name)
  (gethash method-name mapper))

(defun to-app (mapper)
  (lambda (message)
    (let ((handler (find-handler mapper (request-method message))))
      (unless handler
        (error 'jsonrpc-method-not-found))
      (let ((result (apply handler (request-params message))))
        (when (request-id message)
          (make-response :id (request-id message)
                         :result result))))))
