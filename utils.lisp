(in-package #:cl-user)
(defpackage #:jsonrpc/utils
  (:use #:cl)
  (:import-from #:usocket
                #:socket-listen
                #:socket-close
                #:address-in-use-error)
  (:export #:random-port))
(in-package #:jsonrpc/utils)

(defun port-available-p (port)
  (handler-case (let ((socket (usocket:socket-listen "127.0.0.1" port :reuse-address t)))
                  (usocket:socket-close socket))
    (usocket:address-in-use-error (e) (declare (ignore e)) nil)))

(defun random-port ()
  "Return a port number not in use from 50000 to 60000."
  (loop for port from (+ 50000 (random 1000)) upto 60000
        if (port-available-p port)
          return port))
