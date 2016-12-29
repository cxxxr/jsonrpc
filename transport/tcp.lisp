(in-package #:cl-user)
(defpackage #:jsonrpc/transport/tcp
  (:use #:cl
        #:jsonrpc/utils
        #:jsonrpc/errors
        #:jsonrpc/transport/interface)
  (:import-from #:jsonrpc/request-response
                #:parse-message
                #:make-error-response
                #:request-id)
  (:import-from #:usocket)
  (:import-from #:yason)
  (:import-from #:fast-io
                #:make-output-buffer
                #:finish-output-buffer
                #:fast-write-byte)
  (:import-from #:trivial-utf-8
                #:utf-8-bytes-to-string
                #:string-to-utf-8-bytes)
  (:export #:tcp-transport))
(in-package #:jsonrpc/transport/tcp)

(define-condition eof (error) ())

(defclass tcp-transport (transport)
  ((host :accessor tcp-transport-host
         :initarg :host
         :initform "127.0.0.1")
   (port :accessor tcp-transport-port
         :initarg :port
         :initform (random-port))))

(defmethod start-server ((transport tcp-transport))
  (usocket:with-socket-listener (server (tcp-transport-host transport)
                                        (tcp-transport-port transport)
                                        :reuse-address t
                                        :element-type '(unsigned-byte 8))
    (setf (transport-connection transport) server)
    (let ((clients '()))
      (unwind-protect
           (loop
             (setf clients
                   (delete-if-not #'open-stream-p clients :key #'usocket:socket-stream))
             (usocket:wait-for-input (cons server clients)
                                     :timeout 10)
             (when (member (usocket:socket-state server) '(:read :read-write))
               (let ((client (usocket:socket-accept server)))
                 (push client clients)))
             (dolist (socket clients)
               (when (member (usocket:socket-state socket) '(:read :read-write))
                 (handler-case
                     (handle-request transport socket)
                   (eof ()
                     (usocket:socket-close socket)
                     (setf clients (delete socket clients)))))))
        (mapc #'usocket:socket-close clients)))))

(defmethod start-client ((transport tcp-transport))
  (setf (transport-connection transport)
        (usocket:socket-connect (tcp-transport-host transport)
                                (tcp-transport-port transport)
                                :element-type '(unsigned-byte 8)))
  transport)

(defmethod send-message-using-transport ((transport tcp-transport) connection message)
  (let ((json (with-output-to-string (s)
                (yason:encode message s)))
        (stream (usocket:socket-stream connection)))
    (write-sequence
     (string-to-utf-8-bytes
      (format nil
              "Content-Length: ~A~C~C~:*~:*~C~C~A"
              (length json)
              #\Return
              #\Newline
              json))
     stream)
    (force-output stream)))

(defmethod receive-message-using-transport ((transport tcp-transport) connection)
  (let* ((stream (usocket:socket-stream connection))
         (headers (read-headers stream))
         (length (ignore-errors (parse-integer (gethash "content-length" headers)))))
    (when length
      (let ((body (make-array length :element-type '(unsigned-byte 8))))
        (read-sequence body stream)
        ;; TODO: error handling
        (parse-message (utf-8-bytes-to-string body))))))

(defun read-headers (stream)
  (let (header-field
        (headers (make-hash-table :test 'equal)))

    (tagbody
     read-header-field
       (let ((buffer (fast-io:make-output-buffer)))
         ;; The last of headers
         (let ((byte (read-byte stream nil 0)))
           (cond
             ((= byte (char-code #\Return))
              (progn
                (assert (= (read-byte stream nil 0) (char-code #\Linefeed)))
                (go finish)))
             ((= byte 0)
              (go eof))
             (t
              (fast-write-byte byte buffer))))
         (loop for byte of-type (unsigned-byte 8) = (read-byte stream nil 0)
               if (= byte (char-code #\:))
                 do (setf header-field
                          (string-downcase
                           (map 'string #'code-char (fast-io:finish-output-buffer buffer))))
                    (go read-header-value)
               else if (= byte 0)
                      do (go eof)
               else
                 do (fast-write-byte byte buffer)))

     read-header-value
       (let ((buffer (fast-io:make-output-buffer)))
         (let ((byte (read-byte stream nil 0)))
           (unless (= byte (char-code #\Space))
             (fast-io:fast-write-byte byte buffer)))
         (loop for byte of-type (unsigned-byte 8) = (read-byte stream nil 0)
               if (= byte 0)
                 do (go eof)
               else if (= byte (char-code #\Return))
                      ;; FIXME: The same header field can be found and should be concatenated into the same value
                      do (setf (gethash header-field headers)
                               (map 'string #'code-char (fast-io:finish-output-buffer buffer)))
                         (go read-lf)
               else
                 do (fast-write-byte byte buffer)
               until (= byte (char-code #\Return))))

     read-lf
       (let ((byte (read-byte stream nil 0)))
         (assert (= byte (char-code #\Linefeed)))
         (go read-header-field))

     eof
       (error 'eof)

     finish)

    headers))
