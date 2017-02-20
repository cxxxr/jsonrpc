(in-package #:cl-user)
(defpackage #:jsonrpc/client
  (:use #:cl)
  (:import-from #:jsonrpc/transports
                #:tcp-transport
                #:start-client)
  (:import-from #:jsonrpc/utils
                #:find-mode-class)
  (:import-from #:alexandria
                #:remove-from-plist)
  (:export #:client-connect))
(in-package #:jsonrpc/client)

(defun client-connect (&rest initargs &key (mode :tcp))
  (let ((class (find-mode-class mode))
        (initargs (remove-from-plist initargs :mode)))
    (start-client (apply #'make-instance class initargs))))
