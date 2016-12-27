(in-package #:cl-user)
(defpackage #:jsonrpc/server
  (:use #:cl
        #:jsonrpc/utils)
  (:import-from #:jsonrpc/transports
                #:tcp-transport
                #:start-server
                #:transport-clients)
  (:export #:start-server))
(in-package #:jsonrpc/server)

(defun server-listen (app)
  (let* ((port (random-port))
         (transport (make-instance 'tcp-transport :app app :port port)))
    (start-server transport)))
