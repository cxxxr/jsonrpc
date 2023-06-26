(defpackage #:jsonrpc/transport/http
  (:use #:cl
        #:jsonrpc/transport/interface
        #:jsonrpc/utils)
  (:import-from #:jsonrpc/errors
                #:jsonrpc-error)
  (:import-from #:cl-ppcre)
  (:import-from #:dexador)
  (:import-from #:jsonrpc/connection
                #:connection
                #:connection-socket
                #:add-message-to-queue)
  (:import-from #:jsonrpc/request-response
                #:response-result
                #:response-error-code
                #:response-error-message
                #:response-error
                #:make-request
                #:parse-message)
  (:import-from #:clack)
  (:import-from #:clack.handler.hunchentoot)
  (:import-from #:lack.request)
  (:import-from #:babel
                #:octets-to-string)
  (:import-from #:http-body.util
                #:detect-charset)
  (:import-from #:jsonrpc/transport/interface
                #:run-processing-loop
                #:send-message-using-transport
                #:start-client)
  (:import-from #:jsonrpc/utils
                #:make-id)
  (:import-from #:jsonrpc/class
                #:*default-timeout*
                #:client)
  (:export #:make-clack-app
           #:http-transport))
(in-package #:jsonrpc/transport/http)


(defclass http-transport (transport)
  ((url :accessor http-transport-url
        :initarg :url
        :initform nil)
   (headers :initarg :headers
            :initform nil
            :type list
            :documentation "Alist with additional HTTP headers to be sent in each request. Use it to add Authorization header, for example."
            :reader http-transport-headers))
  (:documentation "This transport allows to access API via HTTP requests.

Here is example, how to setup connection when you need to pass authorization header:

    (defun connect (client &optional token)
      (jsonrpc:client-connect client :mode :http :url \"http://localhost:8001/\"
                                     :headers (when token
                                                (list (cons :authorization
                                                            token))))
      (values client))

"))


(defclass http-connection ()
  ((url :initarg :url
        :reader connection-url)
   (headers :initarg :headers
            :reader connection-headers)))


(defun make-error-response (code message &key (http-code 500))
  (let* ((response (list (cons "code" code)
                         (cons "message" message)))
         (json (with-output-to-string (s)
                 (yason:encode-alist response s))))
    (list http-code
          (list :content-type "application/json"
                :allow-origin "*"
                :access-control-allow-origin "*")
          (list json))))


(defun process-http-request (callback env)
  (let* ((request (lack.request:make-request env)))
    (case (lack.request:request-method request)
      (:options
       ;; OpenRPC Playground (https://playground.open-rpc.org/) and some other tools
       ;; may probe for allowed HTTP methods and headers.
       (list 200
             (list :access-control-allow-origin "*"
                   :access-control-allow-methods "POST"
                   :access-control-allow-headers "Content-Type"
                   :content-type "plain/text")
             (list "")))
      (:post
       ;; Initially I wanted to hack PARSE-MESSAGE to support stream
       ;; retrieved by (lack.request:request-raw-body request)
       ;; but lack returns CIRCULAR-STREAMS:CIRCULAR-INPUT-STREAM
       ;; which does not support SB-GRAY:STREAM-PEEK-CHAR method.
       (let* ((raw-content (lack.request:request-content request))
              (content-type (lack.request:request-content-type request)))
         (cond
           ((cl-ppcre:all-matches "^application/json($|;)" content-type)
            (let* ((content (octets-to-string raw-content
                                              :encoding (detect-charset content-type :utf-8)))
                   (message (handler-case (parse-message content)
                              (jsonrpc-error ()
                                ;; Nothing can be done
                                nil))))
              (cond
                (message
                 (let* ((response (funcall callback message))
                        (json (with-output-to-string (s)
                                (yason:encode response s)))
                        (error-code (jsonrpc/request-response:response-error-code response)))
                   (list (if error-code
                             ;; This mapping was take from the spec:
                             ;; https://www.jsonrpc.org/historical/json-rpc-over-http.html#response-codes
                             (case error-code
                               (-32600 400)
                               (-32601 404)
                               (t 500))
                             200)
                         (list :content-type "application/json"
                               :allow-origin "*"
                               :access-control-allow-origin "*")
                         (list json))))
                (t
                 (make-error-response -32700 "Unable to parse input message.")))))
           (t
            ;; How about using LOG4CL for logging important events?
            ;; (log:error "Content type ~S is not supported" content-type)
            (make-error-response -32700 (format nil "Content type \"~A\" is not supported"
                                                content-type))))))
      (:get
       ;; (log:error "GET is not supported")
       (make-error-response -32700 "GET is not supported")))))


(defun make-clack-app (transport)
  (flet ((json-rpc-http-app (env)
           (process-http-request (transport-message-callback transport) env)))
    #'json-rpc-http-app))


(defmethod start-client ((transport http-transport))
  (let ((connection (make-instance 'http-connection
                                   :url (http-transport-url transport)
                                   :headers (http-transport-headers transport))))
    (setf (transport-connection transport)
          connection)))


(defun merge-headers (&rest header-alists)
  (loop with result = nil
        for headers in header-alists
        do (loop for (key . value) in headers
                 unless (assoc key result :test #'string-equal)
                   do (push (cons key value)
                            result))
        finally (return result)))


(defmethod jsonrpc:call-to ((from client) (to http-connection) (method string) &optional params &rest options)
  "It is possible to pass HTTP headers for this one call by giving an alist as :headers keyword argument."
  (destructuring-bind (&key
		       (timeout *default-timeout*)
		       (basic-auth nil)
		       (headers nil)) options
    (let* ((request (make-request :id (make-id)
                                  :method method
                                  :params params))
           (request-headers (merge-headers
                             (list (cons :content-type "application/json"))
                             (connection-headers to)
                             ;; Here we allow to redefine connection headers for single call
                             headers))
           (json (with-output-to-string (s)
                   (yason:encode request s)))
           (raw-response (dex:post (connection-url to)
                                   :content json
                                   :basic-auth basic-auth
                                   :headers request-headers
                                   :connect-timeout timeout
                                   :read-timeout timeout))
           (response (parse-message raw-response)))

      (if (response-error response)
          (error 'jsonrpc-callback-error
                 :message (response-error-message response)
                 :code (response-error-code response))
          (response-result response)))))
