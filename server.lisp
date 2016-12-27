(in-package #:cl-user)
(defpackage #:jsonrpc/server
  (:use #:cl
        #:jsonrpc/utils)
  (:import-from #:jsonrpc/transports
                #:tcp-transport
                #+nil #:start-server
                #:transport-clients)
  (:export #:start-server))
(in-package #:jsonrpc/server)

(defun start-server (app)
  (let* ((port (random-port))
         (transport (make-instance 'tcp-transport :app app :port port)))
    (jsonrpc/transports:start-server transport)))
