(in-package #:cl-user)
(defpackage #:jsonrpc/client
  (:use #:cl)
  (:import-from #:jsonrpc/transports
                #:tcp-transport
                #:start-client)
  (:export #:client-connect))
(in-package #:jsonrpc/client)

(defun client-connect (&rest initargs)
  (let ((transport (apply #'make-instance 'tcp-transport initargs)))
    (start-client transport)))
