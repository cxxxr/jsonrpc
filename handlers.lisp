(in-package #:cl-user)
(uiop:define-package #:jsonrpc/handlers
    (:use-reexport #:jsonrpc/handler/interface
                   #:jsonrpc/handler/tcp))
