(in-package #:cl-user)
(defpackage #:jsonrpc/server
  (:use #:cl)
  (:import-from #:jsonrpc/server/mapper
                #:to-app)
  (:import-from #:jsonrpc/transports
                #:tcp-transport
                #:start-server)
  (:import-from #:alexandria
                #:remove-from-plist)
  (:export #:server-listen))
(in-package #:jsonrpc/server)

(defun server-listen (mapper &rest initargs &key (mode :tcp) &allow-other-keys)
  (let ((class
          (ecase mode
            (:tcp 'jsonrpc/transport/tcp:tcp-transport)
            (:stdio 'jsonrpc/transport/stdio:stdio)))
        (initargs (remove-from-plist initargs :mode)))
    (start-server (apply #'make-instance class :app (to-app mapper) initargs))))
