(defpackage #:jsonrpc/transport/stdio
  (:use #:cl
        #:jsonrpc/transport/interface)
  (:import-from #:jsonrpc/connection
                #:connection
                #:connection-stream)
  (:import-from #:yason)
  (:import-from #:bordeaux-threads
                #:make-thread
                #:destroy-thread)
  (:import-from #:jsonrpc/request-response
                #:write-message
                #:read-message)
  (:export #:stdio-transport))
(in-package #:jsonrpc/transport/stdio)

(defclass stdio-transport (transport)
  ((input :type stream
          :initarg :input
          :initform *standard-input*
          :accessor stdio-transport-input)
   (output :type stream
           :initarg :output
           :initform *standard-output*
           :accessor stdio-transport-output)))

(defmethod start-server ((transport stdio-transport))
  (let* ((stream (make-two-way-stream (stdio-transport-input transport)
                                      (stdio-transport-output transport)))
         (connection (make-instance 'connection
                                    :stream stream
                                    :request-callback (transport-message-callback transport))))
    (setf (transport-connection transport) connection)
    (let ((thread
            (bt2:make-thread
             (lambda ()
               (run-processing-loop transport connection))
             :name "jsonrpc/transport/stdio processing")))
      (unwind-protect (run-reading-loop transport connection)
        (bt2:destroy-thread thread)))))

(defmethod start-client ((transport stdio-transport))
  (let* ((stream (make-two-way-stream (stdio-transport-input transport)
                                      (stdio-transport-output transport)))
         (connection (make-instance 'connection
                                    :stream stream
                                    :request-callback (transport-message-callback transport))))
    (setf (transport-connection transport) connection)

    (setf (transport-threads transport)
          (list
           (bt2:make-thread
            (lambda ()
              (run-processing-loop transport connection))
            :name "jsonrpc/transport/stdio processing")
           (bt2:make-thread
            (lambda ()
              (run-reading-loop transport connection))
            :name "jsonrpc/transport/stdio reading")))
    connection))

(defmethod send-message-using-transport ((transport stdio-transport) connection message)
  (write-message message (connection-stream connection)))

(defmethod receive-message-using-transport ((transport stdio-transport) connection)
  (read-message (connection-stream connection)))
