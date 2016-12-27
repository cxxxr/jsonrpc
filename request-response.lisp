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
           #:parse-request
           #:parse-response))
(in-package #:jsonrpc/request-response)

(defstruct request
  method
  params
  id)

(defstruct response
  error
  result
  id)

(defun parse-request (input)
  (labels ((validate (request)
             (unless (and (equal (gethash "jsonrpc" request) "2.0")
                          (stringp (gethash "method" request))
                          (typep (gethash "params" request)
                                 '(or hash-table array null))
                          (typep (gethash "id" request)
                                 '(or string number null))
                          (every (lambda (key)
                                   (find key '("jsonrpc" "method" "params" "id") :test #'string=))
                                 (hash-table-keys request)))
               (error 'jsonrpc-invalid-request)))
           (make-req (request)
             (validate request)
             (make-request :method (gethash "method" request)
                           :params (gethash "params" request)
                           :id (gethash "id" request))))
    (let ((request (handler-case (yason:parse input)
                     (error () (error 'jsonrpc-parse-error)))))
      (etypecase request
        (array
         (loop for req across request
               collect (make-req req)))
        (hash-table
         (make-req request))))))

(defun parse-response (input)
  (labels ((validate (response)
             (unless (and (equal (gethash "jsonrpc" response) "2.0")
                          (typep (gethash "error" response)
                                 '(or null hash-table))
                          (typep (gethash "id" response)
                                 '(or string number null))
                          (xor (nth-value 1 (gethash "error" response))
                               (nth-value 1 (gethash "result" response)))
                          (every (lambda (key)
                                   (find key '("jsonrpc" "result" "error" "id") :test #'string=))
                                 (hash-table-keys response)))
               (error 'jsonrpc-invalid-response)))
           (make-res (response)
             (validate response)
             (make-response :result (gethash "result" response)
                            :error (gethash "error" response)
                            :id (gethash "id" response))))
    (let ((response (handler-case (yason:parse input)
                      (error () (error 'jsonrpc-parse-error)))))
      (etypecase response
        (array
         (loop for res across response
               collect (make-res res)))
        (hash-table
         (make-res response))))))

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
