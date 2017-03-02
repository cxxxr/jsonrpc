(in-package #:cl-user)
(defpackage #:jsonrpc/transport/interface
  (:use #:cl)
  (:import-from #:jsonrpc/connection
                #:next-request
                #:process-request
                #:add-message-to-queue)
  (:import-from #:bordeaux-threads)
  (:import-from #:dissect)
  (:export #:transport
           #:transport-message-callback
           #:transport-connection
           #:start-server
           #:start-client
           #:send-message-using-transport
           #:receive-message-using-transport
           #:run-processing-loop
           #:run-reading-loop))
(in-package #:jsonrpc/transport/interface)

(defclass transport ()
  ((message-callback :initarg :message-callback
                     :accessor transport-message-callback)
   (connection :accessor transport-connection)))

(defgeneric start-server (transport))

(defgeneric start-client (transport))

(defgeneric send-message-using-transport (transport to message))

(defgeneric receive-message-using-transport (transport from))

(defgeneric run-processing-loop (transport connection)
  (:method ((transport transport) connection)
    (loop
      (let* ((request (next-request connection))
             (response (process-request connection request)))
        (when response
          (send-message-using-transport transport connection response))))))

(defgeneric run-reading-loop (transport connection)
  (:method ((transport transport) connection)
    (loop for message = (receive-message-using-transport transport connection)
          while message
          do (add-message-to-queue connection message))))
