(in-package #:cl-user)
(defpackage #:jsonrpc/class
  (:use #:cl)
  (:import-from #:jsonrpc/mapper
                #:make-mapper
                #:to-app
                #:register-method-to-mapper)
  (:import-from #:jsonrpc/transport/interface
                #:transport
                #:start-server
                #:start-client
                #:send-message
                #:receive-message)
  (:import-from #:jsonrpc/utils
                #:find-mode-class)
  (:import-from #:alexandria
                #:remove-from-plist)
  (:export #:client
           #:server
           #:jsonrpc-transport
           #:register-method
           #:server-listen
           #:client-connect))
(in-package #:jsonrpc/class)

(defclass jsonrpc ()
  ((mapper :initform (make-mapper)
           :reader jsonrpc-mapper)
   (transport :type (or null transport)
              :initarg :transport
              :initform nil
              :accessor jsonrpc-transport)))

(defclass client (jsonrpc) ())

(defclass server (jsonrpc) ())

(defgeneric register-method (object method-name function)
  (:method ((object jsonrpc) method-name function)
    (register-method-to-mapper (jsonrpc-mapper object)
                               method-name function)))

(defgeneric server-listen (server &rest initargs &key mode &allow-other-keys)
  (:method ((server server) &rest initargs &key (mode :tcp) &allow-other-keys)
    (let* ((class (find-mode-class mode))
           (initargs (remove-from-plist initargs :mode)))
      (unless class
        (error "Unknown mode ~A" mode))
      (let ((transport (apply #'make-instance class
                              :message-callback 
                              (to-app (jsonrpc-mapper server))
                              initargs)))
        (setf (jsonrpc-transport server) transport)
        (start-server transport)))
    server))

(defgeneric client-connect (client &rest initargs &key mode &allow-other-keys)
  (:method ((client client) &rest initargs &key (mode :tcp) &allow-other-keys)
    (let* ((class (find-mode-class mode))
           (initargs (remove-from-plist initargs :mode)))
      (unless class
        (error "Unknown mode ~A" mode))
      (let ((transport (apply #'make-instance class
                              :message-callback 
                              (to-app (jsonrpc-mapper client))
                              initargs)))
        (setf (jsonrpc-transport client) transport)
        (start-client transport)))
    client))

(defmethod send-message ((to jsonrpc) connection message)
  (send-message (jsonrpc-transport to) connection message))

(defmethod receive-message ((from jsonrpc) connection)
  (receive-message (jsonrpc-transport from) connection))
