(in-package #:cl-user)
(defpackage #:jsonrpc/request-response
  (:use #:cl
        #:jsonrpc/errors)
  (:import-from #:yason
                #:parse
                #:encode-object
                #:with-object
                #:encode-object-element)
  (:import-from #:alexandria
                #:hash-table-keys
                #:xor)
  (:export #:request
           #:response
           #:make-request
           #:make-response
           #:request-method
           #:request-params
           #:request-id
           #:response-error
           #:response-result
           #:response-id
           #:parse-message))
(in-package #:jsonrpc/request-response)

(defstruct request
  method
  params
  id)

(defstruct response
  error
  result
  id)

(defun valid-request-p (request)
  (and (equal (gethash "jsonrpc" request) "2.0")
       (stringp (gethash "method" request))
       (typep (gethash "params" request)
              '(or hash-table list))
       (typep (gethash "id" request)
              '(or string number null))
       (every (lambda (key)
                (find key '("jsonrpc" "method" "params" "id") :test #'string=))
              (hash-table-keys request))))

(defun valid-response-p (response)
  (and (equal (gethash "jsonrpc" response) "2.0")
       (typep (gethash "error" response)
              '(or null hash-table))
       (typep (gethash "id" response)
              '(or string number null))
       (xor (nth-value 1 (gethash "error" response))
            (nth-value 1 (gethash "result" response)))
       (every (lambda (key)
                (find key '("jsonrpc" "result" "error" "id") :test #'string=))
              (hash-table-keys response))))

(defun parse-message (input)
  (let ((message (handler-case (yason:parse input)
                   (error () (error 'jsonrpc-parse-error)))))
    (flet ((make-message (hash)
             (if (gethash "method" hash)
                 (progn
                   (unless (valid-request-p hash)
                     (error 'jsonrpc-invalid-request))
                   (make-request :method (gethash "method" hash)
                                 :params (gethash "params" hash)
                                 :id (gethash "id" hash)))
                 (progn
                   (unless (valid-response-p hash)
                     (error 'jsonrpc-invalid-response))
                   (make-response :result (gethash "result" hash)
                                  :error (gethash "error" hash)
                                  :id (gethash "id" hash))))))
      (etypecase message
        (array
         (map 'list #'make-message message))
        (hash-table
         (make-message message))))))

(defmethod yason:encode-object ((request request))
  (yason:with-object ()
    (yason:encode-object-element "jsonrpc" "2.0")
    (yason:encode-object-element "method" (request-method request))
    (yason:encode-object-element "params" (request-params request))
    (when (request-id request)
      (yason:encode-object-element "id" (request-id request)))))

(defmethod yason:encode-object ((response response))
  (yason:with-object ()
    (yason:encode-object-element "jsonrpc" "2.0")
    (cond
      ((response-error response)
       (yason:encode-object-element "error" (response-error response)))
      ((response-result response)
       (yason:encode-object-element "result" (response-result response))))
    (yason:encode-object-element "id" (response-id response))))
