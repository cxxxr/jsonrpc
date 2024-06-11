(defpackage :jsonrpc/base
  (:use :cl)
  (:import-from #:jsonrpc/mapper
                #:exposable)
  (:import-from #:jsonrpc/transport/interface
                #:transport)
  (:export :jsonrpc
           :jsonrpc-transport
           :ensure-connected))
(in-package :jsonrpc/base)

(defclass jsonrpc (exposable)
  ((transport :type (or null transport)
              :initarg :transport
              :initform nil
              :accessor jsonrpc-transport)))

(defun ensure-connected (jsonrpc)
  (check-type jsonrpc jsonrpc)
  (unless (jsonrpc-transport jsonrpc)
    (error "Connection isn't established yet for ~A" jsonrpc)))
