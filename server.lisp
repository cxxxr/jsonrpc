(in-package #:cl-user)
(defpackage #:jsonrpc/server
  (:use #:cl)
  (:import-from #:jsonrpc/transports
                #:tcp-transport
                #:start-server)
  (:export #:server-listen))
(in-package #:jsonrpc/server)

(defun server-listen (app &rest initargs)
  (let ((transport (apply #'make-instance 'tcp-transport :app app initargs)))
    (start-server transport)))
