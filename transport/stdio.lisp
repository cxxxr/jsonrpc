(in-package #:cl-user)
(defpackage #:jsonrpc/transport/stdio
  (:use #:cl
        #:jsonrpc/transport/interface)
  (:import-from #:jsonrpc/connection
                #:connection
                #:connection-socket)
  (:import-from #:yason)
  (:import-from #:bordeaux-threads
                #:make-thread
                #:destroy-thread)
  (:import-from #:jsonrpc/request-response
                #:parse-message)
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
                                    :socket stream
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
                                    :socket stream
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
  (let ((json (with-output-to-string (s)
                (yason:encode message s)))
        (stream (connection-socket connection)))
    (format stream "Content-Length: ~A~C~C~:*~:*~C~C~A"
            (length json)
            #\Return
            #\Newline
            json)
    (force-output stream)))

(defmethod receive-message-using-transport ((transport stdio-transport) connection)
  (let* ((stream (connection-socket connection))
         (headers (read-headers stream))
         (length (ignore-errors (parse-integer (gethash "content-length" headers)))))
    (when length
      (let ((body (make-string length)))
        (read-sequence body (stdio-transport-input transport))
        (parse-message body)))))

;; character stream
(defun read-headers (stream)
  (let ((headers (make-hash-table :test 'equal)))
    (loop for line = (read-line stream)
          until (equal (string-trim '(#\Return #\Newline) line) "")
          do (let* ((colon-pos (position #\: line))
                    (field (string-downcase (subseq line 0 colon-pos)))
                    (value (string-trim '(#\Return #\Space #\Tab) (subseq line (1+ colon-pos)))))
               (setf (gethash field headers) value)))
    headers))
