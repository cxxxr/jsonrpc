(in-package #:cl-user)
(defpackage #:jsonrpc/server
  (:use #:cl)
  (:import-from #:jsonrpc/handlers
                #:tcp-handler
                #:start-handler
                #:handler-clients)
  (:export #:start-server))
(in-package #:jsonrpc/server)

(defun start-server (app)
  (let ((handler (make-instance 'tcp-handler :app app)))
    (start-handler handler)))
