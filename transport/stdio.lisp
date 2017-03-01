(in-package #:cl-user)
(defpackage #:jsonrpc/transport/stdio
  (:use #:cl
        #:jsonrpc/transport/interface)
  (:import-from #:yason)
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
  (let ((stream (make-two-way-stream (stdio-transport-input transport)
                                     (stdio-transport-output transport))))
    (setf (transport-connection transport) stream)
    (loop for message = (receive-message transport stream)
          while message
          do (handle-message transport stream message))))

(defmethod start-client ((transport stdio-transport))
  (loop))

(defmethod send-message ((transport stdio-transport) stream message)
  (let ((json (with-output-to-string (s)
                (yason:encode message s))))
    (format stream "Content-Length: ~A~C~C~:*~:*~C~C~A"
            (length json)
            #\Return
            #\Newline
            json)
    (force-output stream)))

(defmethod receive-message ((transport stdio-transport) stream)
  (let* ((headers (read-headers stream))
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
