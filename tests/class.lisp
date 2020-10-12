(in-package #:cl-user)
(defpackage #:jsonrpc/tests/class
  (:use #:cl
        #:rove
        #:jsonrpc/request-response
        #:jsonrpc/errors)
  (:import-from #:bordeaux-threads)
  (:shadowing-import-from #:rove
                          #:*debug-on-error*))
(in-package #:jsonrpc/tests/class)

(defvar *port-number* 56789)

(defun server-running-p (port)
  (handler-case (let ((socket (usocket:socket-connect "127.0.0.1" port)))
                  (usocket:socket-close socket)
                  t)
    (usocket:connection-refused-error () nil)))

(defun make-server (port mode method-name function)
  (bt:make-thread
   (lambda ()
     (let ((server (jsonrpc:make-server)))
       (jsonrpc:expose server method-name function)
       (jsonrpc:server-listen server :port port :mode mode)))))

(defun wait-server (port)
  (loop do (sleep 0.1)
        until (server-running-p port)))

(define-condition dummy-server-error (jsonrpc-error) ())

(deftest server-error-test
  (dolist (mode '(:websocket))
    (let ((server-thread (make-server (incf *port-number*)
                                      mode
                                      "error"
                                      (lambda (args)
                                        (declare (ignore args))
                                        (error 'dummy-server-error))))
          (client (jsonrpc:make-client)))
      (unwind-protect
           (progn
             (case mode
               (:websocket (wait-server *port-number*)))
             (jsonrpc:client-connect client
                                     :url (case mode
                                            (:websocket (format nil "ws://127.0.0.1:~a" *port-number*)))
                                     :mode mode)
             (handler-case (progn
                             (jsonrpc:call client "error")
                             (fail "must server error"))
               (jsonrpc-error ()
                 (pass "server error"))))
        (bt:destroy-thread server-thread)
        (jsonrpc:client-disconnect client)))))

(deftest timeout-test
  (dolist (mode '(:websocket))
    (let ((server-thread (make-server (incf *port-number*)
                                      mode
                                      "timeout"
                                      (lambda (args)
                                        (declare (ignore args))
                                        (loop (sleep 1)))))
          (client (jsonrpc:make-client)))
      (unwind-protect
           (progn
             (case mode
               (:websocket (wait-server *port-number*)))
             (jsonrpc:client-connect client
                                     :url (case mode
                                            (:websocket (format nil "ws://127.0.0.1:~a" *port-number*)))
                                     :mode mode)
             (handler-case (progn
                             (jsonrpc:call client "timeout" nil :timeout 1)
                             (fail "must timeout"))
               (jsonrpc-timeout ()
                 (pass "timeout"))))
        (bt:destroy-thread server-thread)
        (jsonrpc:client-disconnect client)))))
