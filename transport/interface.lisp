(defpackage #:jsonrpc/transport/interface
  (:use #:cl)
  (:import-from #:jsonrpc/connection
                #:wait-for-ready
                #:process-request
                #:add-message-to-queue
                #:connection-request-queue
                #:connection-outbox
                #:add-message-to-outbox)
  (:import-from #:bordeaux-threads)
  (:import-from #:chanl)
  (:export #:transport
           #:transport-jsonrpc
           #:transport-message-callback
           #:transport-connection
           #:transport-threads
           #:start-server
           #:start-client
           #:send-message-using-transport
           #:receive-message-using-transport
           #:run-processing-loop
           #:run-reading-loop
           #:find-mode-class))
(in-package #:jsonrpc/transport/interface)

(defclass transport ()
  ((jsonrpc :initarg :jsonrpc :reader transport-jsonrpc) ; server or client
   (message-callback :initarg :message-callback
                     :accessor transport-message-callback)
   (connection :accessor transport-connection)
   (threads :initform '()
            :accessor transport-threads)))

(defgeneric start-server (transport))

(defgeneric start-client (transport))

(defgeneric send-message-using-transport (transport to message))

(defgeneric receive-message-using-transport (transport from))

(defgeneric run-processing-loop (transport connection)
  (:method ((transport transport) connection)
    (let ((request-queue (connection-request-queue connection))
          (outbox (connection-outbox connection)))
      (loop
        (wait-for-ready connection)
        (chanl:select
          ((chanl:recv request-queue request)
           (let ((response (process-request connection request)))
             (when response
               (add-message-to-outbox connection response))))
          ((chanl:recv outbox message)
           (send-message-using-transport transport connection message)))))))

(defgeneric run-reading-loop (transport connection)
  (:method ((transport transport) connection)
    (loop for message = (receive-message-using-transport transport connection)
          while message
          do (add-message-to-queue connection message))))

(defvar *transport-load-lock* (bt:make-recursive-lock))
(defun find-mode-class (mode)
  (let ((system-name (format nil "jsonrpc/transport/~(~A~)" mode))
        (package-name (format nil "~A/~A"
                              :jsonrpc/transport
                              mode)))

    (let ((package
            (bt:with-lock-held (*transport-load-lock*)
              (or (find-package package-name)
                  (progn
                    #+quicklisp
                    (ql:quickload system-name :silent t)
                    #-quicklisp
                    (asdf:load-system system-name :verbose nil)
                    (find-package package-name))))))
      (and package
           (find-class (intern (format nil "~A-~A" mode :transport) package))))))
