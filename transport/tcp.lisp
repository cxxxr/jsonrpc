(in-package #:cl-user)
(defpackage #:jsonrpc/transport/tcp
  (:use #:cl
        #:jsonrpc/utils
        #:jsonrpc/transport/interface)
  (:import-from #:jsonrpc/request-response
                #:parse-request)
  (:import-from #:usocket)
  (:import-from #:yason)
  (:import-from #:fast-io
                #:make-output-buffer
                #:finish-output-buffer
                #:fast-write-byte)
  (:import-from #:trivial-utf-8
                #:utf-8-bytes-to-string)
  (:export #:tcp-transport))
(in-package #:jsonrpc/transport/tcp)

(define-condition eof (error) ())

(defclass tcp-transport (transport)
  ((socket :initform nil
           :accessor tcp-transport-socket)
   (host :accessor tcp-transport-host
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
    (setf (tcp-transport-socket transport) server)
    (unwind-protect
         (loop
           (setf (transport-clients transport)
                 (remove-if-not #'open-stream-p
                                (transport-clients transport)
                                :key #'usocket:socket-stream))
           (usocket:wait-for-input (cons server
                                         (transport-clients transport))
                                   :timeout 10)
           (when (member (usocket:socket-state server) '(:read :read-write))
             (let ((client (usocket:socket-accept server)))
               (push client (transport-clients transport))))
           (dolist (socket (transport-clients transport))
             (when (member (usocket:socket-state socket) '(:read :read-write))
               (handle-request transport socket))))
      (mapc #'usocket:socket-close (transport-clients transport)))))

(defmethod start-client ((transport tcp-transport))
  (setf (tcp-transport-socket transport)
        (usocket:socket-connect (tcp-transport-host transport)
                                (tcp-transport-port transport)
                                :element-type '(unsigned-byte 8)))
  transport)

(defmethod handle-request ((transport tcp-transport) socket)
  (handler-case
      (funcall (transport-app transport)
               (receive-message-using-transport transport socket))
    (eof ()
      (usocket:socket-close socket)
      (setf (transport-clients transport)
            (remove socket (transport-clients transport))))))

(defmethod send-message-using-transport ((transport tcp-transport) message)
  (let ((json (yason:with-output-to-string* ()
                (yason:encode-object message))))
    (format (usocket:socket-stream (tcp-transport-socket transport))
            "Content-Length: ~A~C~C~:*~:*~C~C~A"
            (length json)
            #\Return
            #\Newline
            json)))

(defmethod receive-message-using-transport ((transport tcp-transport)
                                            &optional (socket (tcp-transport-socket transport)))
  (let* ((stream (usocket:socket-stream socket))
         (headers (read-headers stream))
         (length (ignore-errors (parse-integer (gethash "content-length" headers)))))
    (when length
      (let ((body (make-array length :element-type '(unsigned-byte 8))))
        (read-sequence body stream)
        ;; TODO: error handling
        (parse-request (utf-8-bytes-to-string body))))))

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
