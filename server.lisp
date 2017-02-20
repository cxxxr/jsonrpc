(in-package #:cl-user)
(defpackage #:jsonrpc/server
  (:use #:cl)
  (:import-from #:jsonrpc/server/mapper
                #:to-app)
  (:import-from #:jsonrpc/transports
                #:tcp-transport
                #:stdio-transport
                #:start-server)
  (:import-from #:alexandria
                #:remove-from-plist)
  (:export #:server-listen))
(in-package #:jsonrpc/server)

(defun find-mode-class (mode)
  (let ((package (find-package (format nil "~A/~A"
                                       :jsonrpc/transport
                                       mode))))
    (find-class (intern (format nil "~A-~A" mode :transport) package))))

(defun server-listen (mapper &rest initargs &key (mode :tcp) &allow-other-keys)
  (let ((class (find-mode-class mode))
        (initargs (remove-from-plist initargs :mode)))
    (start-server (apply #'make-instance class :app (to-app mapper) initargs))))
