(in-package #:cl-user)
(defpackage #:jsonrpc/tests/request-response
  (:use #:cl
        #:rove
        #:jsonrpc/request-response
        #:jsonrpc/errors)
  (:shadowing-import-from #:rove
                          #:*debug-on-error*))
(in-package #:jsonrpc/tests/request-response)

(deftest parse-message-test
  (testing "invalid message"
    (ok (signals (parse-message "xxx") 'jsonrpc-parse-error)
        "Parse error")
    (ok (signals (parse-message "{}") '(or jsonrpc-invalid-request jsonrpc-invalid-response))
        "Empty object is invalid"))

  (testing "invalid request"
    (ok (null (parse-message "[]"))
        "Empty array is okay (batch request)")
    (ok (typep (parse-message "{\"method\":\"add\",\"params\":[1,2],\"id\":1}" :version 1.0)
	       'request)
        "\"jsonrpc\" does not need to be supplied for version 1.0")
    (ok (signals (parse-message "{\"method\":\"add\",\"params\":[1,2],\"id\":1}")
            'jsonrpc-invalid-request)
        "\"jsonrpc\" member is missing")
    (ok (typep (parse-message "{\"method\":\"add\",\"jsonrpc\":\"2.0\"}") 'request)
        "\"params\" and \"id\" can be omitted")
    (ok (signals (parse-message "{\"method\":1,\"jsonrpc\":\"2.0\"}")
            'jsonrpc-invalid-request)
        "\"method\" must be a string"))

  (testing "invalid response"
    (ok (signals (parse-message "{\"id\":1,\"result\":3}")
            'jsonrpc-invalid-response)
        "\"jsonrpc\" member is missing")
    (let ((*jsonrpc-version* 1.0))
      (ok (typep (parse-message "{\"id\":1,\"result\":3,\"error\":null}")
            'response)
        "\"jsonrpc\" member does not need to be supplied for version 1.0, but error does."))
    (ok (typep (parse-message "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":3}")
               'response))
    (ok (typep (parse-message "{\"id\":1,\"result\":3,\"error\":{\"code\":-32000,\"message\":\"something wrong\"}}" :version 1.0)
            'response)
        "For Version 1.0 both \"result\" and \"error\" must be specified.")
    (ok (signals (parse-message "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":3,\"error\":{\"code\":-32000,\"message\":\"something wrong\"}}")
            'jsonrpc-invalid-response)
        "Must not to specify both of \"result\" and \"error\"")
    (ok (signals (parse-message "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":\"something wrong\"}")
            'jsonrpc-invalid-response)
        "\"error\" must be a string"))

  (testing "general cases"
    (let ((message (parse-message "{\"method\":\"add\",\"params\":[1,2],\"id\":1,\"jsonrpc\":\"2.0\"}")))
      (ok (typep message 'request)))
    (let ((message (parse-message "{\"method\":\"add\",\"params\":[1,2],\"id\":1}" :version 1.0)))
      (ok (typep message 'request)))
    (let ((message (parse-message "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-32000,\"message\":\"something wrong\"}}")))
      (ok (typep message 'response)))
    (let ((message (parse-message "{\"id\":1,\"result\":null,\"error\":{\"code\":-32000,\"message\":\"something wrong\"}}"
				  :version 1.0)))
      (ok (typep message 'response)))))

(deftest json-encode
  (testing "request"
    (let ((request (make-request :id 1 :method "add" :params '(3 10))))
      (ok (outputs (yason:encode request)
              "{\"jsonrpc\":\"2.0\",\"method\":\"add\",\"params\":[3,10],\"id\":1}")))
    (let ((request (make-request :id 1 :method "add" :params '(3 10) :version 1.0)))
      (ok (outputs (yason:encode request)
              "{\"method\":\"add\",\"params\":[3,10],\"id\":1}"))))
  (testing "response"
    (let ((response (make-response :id 1 :result 13)))
      (ok (outputs (yason:encode response)
              "{\"jsonrpc\":\"2.0\",\"result\":13,\"id\":1}")))
    (let ((response (make-response :id 1 :result 13 :version 1.0)))
      (ok (outputs (yason:encode response)
              "{\"result\":13,\"id\":1}")))
    (let ((response (make-response :id 2 :result nil)))
      (ok (outputs (yason:encode response)
              "{\"jsonrpc\":\"2.0\",\"result\":null,\"id\":2}")))))
