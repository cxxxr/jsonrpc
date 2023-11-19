(in-package #:cl-user)
(defpackage #:jsonrpc/request-response
  (:use #:cl
        #:jsonrpc/errors)
  (:import-from #:yason
                #:with-output
                #:parse
                #:encode
                #:with-object
                #:encode-object-element)
  (:import-from #:alexandria
                #:hash-table-keys
                #:xor)
  (:export #:request
           #:response
	   #:jsonrpc-version
	   #:*jsonrpc-version*
           #:make-request
           #:make-response
           #:make-error-response
           #:request-method
           #:request-params
           #:request-id
           #:response-error
           #:response-error-message
           #:response-error-code
           #:response-result
           #:response-id
           #:parse-message))
(in-package #:jsonrpc/request-response)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (ftype (function (number)
			    (values boolean &optional))
		  json-rpc-version-p))
  (defun json-rpc-version-p (version)
    (when (or (= version 1.0)
	      (= version 2.0))
      t)))

(deftype jsonrpc-version ()
  '(and number (satisfies json-rpc-version-p)))

(declaim (type (jsonrpc-version) *jsonrpc-version*))
(defvar *jsonrpc-version* 2.0
  "Default JSON-RPC version used for request/response encoding/validation.")

(defstruct request
  (version *jsonrpc-version* :type jsonrpc-version)
  method
  params
  id)

(defstruct response
  (version *jsonrpc-version* :type jsonrpc-version)
  error
  result
  id)

(defun make-error-response (&key id code message (data nil data-specified-p))
  (let ((hash (make-hash-table :test 'equal)))
    (setf (gethash "code" hash) code
          (gethash "message" hash) message)
    (when data-specified-p
      (setf (gethash "data" hash) data))
    (make-response :error hash :id id)))

(defun response-error-message (response)
  (let ((error (response-error response)))
    (when error
      (gethash "message" error))))

(defun response-error-code (response)
  (let ((error (response-error response)))
    (when error
      (gethash "code" error))))

(declaim (ftype (function (hash-table &key (:version jsonrpc-version))
			  (values boolean &optional))
		valid-request-p))
(defun valid-request-p (request &key (version *jsonrpc-version*))
  (let ((id (gethash "id" request)))
    (and (stringp (gethash "method" request))
	 (typep id '(or string number null))
	 (ecase version
	   (2.0
	    (and (equal (gethash "jsonrpc" request) "2.0")
		 (typep (gethash "params" request)
			'(or hash-table list))
		 (every (lambda (key)
			  (find key '("jsonrpc" "method" "params" "id") :test #'string=))
			(hash-table-keys request))))
	   (1.0
	    (consp (gethash "params" request))
	    (or id (eql id 'null))
	    (every (lambda (key)
		     (find key '("method" "params" "id") :test #'string=))
		   (hash-table-keys request)))))))

(declaim (ftype (function (hash-table &key (:version jsonrpc-version))
			  (values boolean &optional))
		valid-response-p))
(defun valid-response-p (response &key (version *jsonrpc-version*))
  "Predicate function which returns t, if response is valid and nil if not.
Default rpc-version is 2.0, alternatively 1.0 can be supplied."
  (when (ecase version
	  (2.0
	   (and (string= (gethash "jsonrpc" response) "2.0")
		(typep (gethash "id" response) '(or string number null))
		(typep (gethash "error" response) '(or null hash-table))
		(xor (nth-value 1 (gethash "error" response))
		     (nth-value 1 (gethash "result" response)))
		(every (lambda (key)
			 (find key (list "jsonrpc" "result" "error" "id") :test #'string=))
		       (hash-table-keys response))))
	  (1.0
	   (and (gethash "id" response)
		(typep (gethash "error" response) '(or null hash-table))
		(and (nth-value 1 (gethash "error" response))
		     (nth-value 1 (gethash "result" response)))
		(every (lambda (key)
			 (find key (list "result" "error" "id") :test #'string=))
		       (hash-table-keys response)))))
    t))

(declaim (ftype (function ((or string stream) &key (:version jsonrpc-version))
			  t)
		parse-message))
(defun parse-message (input &key (version *jsonrpc-version*))
  (when (or (and (typep input 'string)
                 (< 0 (length input)))
            (typep input 'stream))
    (let ((message (handler-case (yason:parse input)
                     (error () (error 'jsonrpc-parse-error)))))
      (flet ((make-message (hash)
               (if (gethash "method" hash)
                   (progn
                     (unless (valid-request-p hash :version version)
                       (error 'jsonrpc-invalid-request))
                     (make-request :method (gethash "method" hash)
                                   :params (gethash "params" hash)
				   :version version
                                   :id (gethash "id" hash)))
                   (progn
                     (unless (valid-response-p hash :version version)
                       (error 'jsonrpc-invalid-response))
                     (make-response :result (gethash "result" hash)
                                    :error (gethash "error" hash)
				    :version version
                                    :id (gethash "id" hash))))))
        (etypecase message
          (list
           (mapcar #'make-message message))
          (hash-table
           (make-message message)))))))

(defmethod yason:encode ((request request) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (let ((version (request-version request)))
	(when (= 2.0 version)
	  (yason:encode-object-element "jsonrpc" "2.0")))
      (yason:encode-object-element "method" (request-method request))
      (yason:encode-object-element "params" (request-params request))
      (when (request-id request)
        (yason:encode-object-element "id" (request-id request))))))

(defmethod yason:encode ((response response) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (let ((version (response-version response)))
	(when (= 2.0 version)
	  (yason:encode-object-element "jsonrpc" "2.0")))
      (if (response-error response)
          (yason:encode-object-element "error" (response-error response))
          (yason:encode-object-element "result" (response-result response)))
      (yason:encode-object-element "id" (response-id response)))))
