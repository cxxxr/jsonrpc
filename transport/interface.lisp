(in-package #:cl-user)
(defpackage #:jsonrpc/transport/interface
  (:use #:cl)
  (:import-from #:jsonrpc/connection
                #:process-request
                #:add-message-to-queue
                #:connection-request-queue)
  (:import-from #:bordeaux-threads)
  (:import-from #:event-emitter
                #:event-emitter)
  (:import-from #:chanl)
  (:export #:transport
           #:transport-message-callback
           #:transport-connection
           #:transport-threads
           #:start-server
           #:start-client
           #:send-message-using-transport
           #:receive-message-using-transport
           #:run-processing-loop
           #:run-reading-loop))
(in-package #:jsonrpc/transport/interface)

(defclass transport (event-emitter)
  ((message-callback :initarg :message-callback
                     :accessor transport-message-callback)
   (connection :accessor transport-connection)
   (threads :initform '()
            :accessor transport-threads)
   (send-lock :initform (bt:make-lock)
              :reader transport-send-lock)))

(defgeneric start-server (transport))

(defgeneric start-client (transport))

(defgeneric send-message-using-transport (transport to message)
  (:method :around ((transport transport) to message)
    (bt:with-lock-held ((transport-send-lock transport))
      (call-next-method))))

(defgeneric receive-message-using-transport (transport from))

(defgeneric run-processing-loop (transport connection)
  (:method ((transport transport) connection)
    (loop
      (let* ((request (chanl:recv (connection-request-queue connection)))
             (response (process-request connection request)))
        (when response
          (send-message-using-transport transport connection response))))))

(defgeneric run-reading-loop (transport connection)
  (:method ((transport transport) connection)
    (loop for message = (receive-message-using-transport transport connection)
          while message
          do (add-message-to-queue connection message))))
