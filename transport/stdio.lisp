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
  (:export #:stdio-transport))
(in-package #:jsonrpc/transport/stdio)

(defun open-binary-stdio (fd direction)
  #+sbcl (sb-sys:make-fd-stream fd
                                :input  (eq direction :input)
                                :output (eq direction :output)
                                :element-type '(unsigned-byte 8)
                                :buffering (if (eq direction :input)
                                               :line :none))
  #-sbcl (error "stdio-transport: pass :input/:output binary streams explicitly."))

(defclass stdio-transport (transport)
  ((input :type stream
          :initarg :input
          :initform (open-binary-stdio 0 :input)
          :accessor stdio-transport-input)
   (output :type stream
           :initarg :output
           :initform (open-binary-stdio 1 :output)
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

