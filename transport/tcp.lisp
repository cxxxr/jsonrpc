(defpackage #:jsonrpc/transport/tcp
  (:use #:cl
        #:jsonrpc/utils
        #:jsonrpc/transport/interface)
  (:import-from #:jsonrpc/base
                #:on-open-connection
                #:on-close-connection)
  (:import-from #:jsonrpc/connection
                #:connection
                #:connection-stream)
  (:import-from #:usocket)
  (:import-from #:cl+ssl)
  (:import-from #:quri)
  (:import-from #:yason)
  (:import-from #:bordeaux-threads
                #:make-thread
                #:destroy-thread)
  (:export #:tcp-transport))
(in-package #:jsonrpc/transport/tcp)

(defclass tcp-transport (transport)
  ((host :accessor tcp-transport-host
         :initarg :host
         :initform "127.0.0.1")
   (port :accessor tcp-transport-port
         :initarg :port
         :initform (random-port))
   (securep :accessor tcp-transport-secure-p
            :initarg :securep
            :initform nil)))

(defmethod initialize-instance :after ((transport tcp-transport) &rest initargs &key url &allow-other-keys)
  (declare (ignore initargs))
  (when url
    (let ((uri (quri:uri url)))
      (unless (quri:uri-http-p uri)
        (error "Only http or https are supported for tcp-transport (specified ~S)" (quri:uri-scheme uri)))
      (setf (tcp-transport-secure-p transport)
            (equalp (quri:uri-scheme uri) "https"))
      (setf (tcp-transport-host transport) (quri:uri-host uri))
      (setf (tcp-transport-port transport) (quri:uri-port uri))))
  transport)

(defmethod start-server ((transport tcp-transport))
  (usocket:with-socket-listener (server (tcp-transport-host transport)
                                        (tcp-transport-port transport)
                                        :reuse-address t
                                        :element-type '(unsigned-byte 8))
    (let ((callback (transport-message-callback transport))
          (client-threads '())
          (bt2:*default-special-bindings* (append bt2:*default-special-bindings*
                                                 `((*standard-output* . ,*standard-output*)
                                                   (*error-output* . ,*error-output*)))))
      (unwind-protect
           (loop
             (usocket:wait-for-input (list server) :timeout 10)
             (when (member (usocket:socket-state server) '(:read :read-write))
               (let* ((socket (usocket:socket-accept server))
                      (connection (make-instance 'connection
                                                 :stream (usocket:socket-stream socket)
                                                 :request-callback callback)))
                 (setf (transport-connection transport) connection)
                 (on-open-connection (transport-jsonrpc transport) connection)
                 (push
                  (bt2:make-thread
                   (lambda ()
                     (let ((thread
                             (bt2:make-thread
                              (lambda ()
                                (run-processing-loop transport connection))
                              :name "jsonrpc/transport/tcp processing"
                              :initial-bindings
                              `((*standard-output* . ,*standard-output*)
                                (*error-output* . ,*error-output*)))))
                       (unwind-protect
                            (run-reading-loop transport connection)
                         (finish-output (connection-stream connection))
                         (usocket:socket-close socket)
                         (bt2:destroy-thread thread)
                         (on-close-connection (transport-jsonrpc transport) connection))))
                   :name "jsonrpc/transport/tcp reading")
                  client-threads))))
        (mapc #'bt2:destroy-thread client-threads)))))

(defmethod start-client ((transport tcp-transport))
  (let ((stream (usocket:socket-stream
                 (usocket:socket-connect (tcp-transport-host transport)
                                         (tcp-transport-port transport)
                                         :element-type '(unsigned-byte 8)))))
    (setf stream
          (if (tcp-transport-secure-p transport)
              (cl+ssl:make-ssl-client-stream stream
                                             :hostname (tcp-transport-host transport))
              stream))

    (let ((connection (make-instance 'connection
                                     :stream stream
                                     :request-callback
                                     (transport-message-callback transport)))
          (bt2:*default-special-bindings* (append bt2:*default-special-bindings*
                                                 `((*standard-output* . ,*standard-output*)
                                                   (*error-output* . ,*error-output*)))))
      (setf (transport-connection transport) connection)

      (on-open-connection (transport-jsonrpc transport) connection)

      (setf (transport-threads transport)
            (list
             (bt2:make-thread
              (lambda ()
                (run-processing-loop transport connection))
              :name "jsonrpc/transport/tcp processing")

             (bt2:make-thread
              (lambda ()
                (run-reading-loop transport connection))
              :name "jsonrpc/transport/tcp reading")))

      connection)))

