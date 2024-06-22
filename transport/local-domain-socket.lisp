(uiop:define-package #:jsonrpc/transport/local-domain-socket
  (:use #:cl
        #:jsonrpc/transport/interface)
  (:import-from #:jsonrpc/base
                #:on-open-connection
                #:on-close-connection)
  (:import-from #:jsonrpc/connection
                #:connection
                #:connection-stream)
  (:import-from #:jsonrpc/request-response
                #:write-message
                #:read-message))
(in-package #:jsonrpc/transport/local-domain-socket)

(defconstant +backlog+ 128)

(defclass local-domain-socket-transport (transport)
  ((address :initarg :address
            :reader local-domain-socket-transport-address)))

(defun socket-listen (address)
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (sb-bsd-sockets:socket-bind socket address)
    (sb-bsd-sockets:socket-listen socket +backlog+)
    socket))

(defmethod start-server ((transport local-domain-socket-transport))
  (let ((listen-socket (socket-listen (local-domain-socket-transport-address transport)))
        (threads '()))
    (unwind-protect
         (loop :for socket := (sb-bsd-sockets:socket-accept listen-socket)
               :for stream := (sb-bsd-sockets:socket-make-stream socket
                                                                 :input t
                                                                 :output t
                                                                 :buffering :full)
               :for connection := (make-instance 'connection
                                                 :stream stream
                                                 :request-callback (transport-message-callback
                                                                    transport))
               :do (setf (transport-connection transport) connection)
                   (on-open-connection (transport-jsonrpc transport) connection)
                   (push (bt2:make-thread
                          (lambda ()
                            (let ((thread
                                    (bt2:make-thread
                                     (lambda ()
                                       (run-processing-loop transport connection))
                                     :name "jsonrpc/transport/local-domain-socket processing")))
                              (unwind-protect (run-reading-loop transport connection)
                                (finish-output (connection-stream connection))
                                (sb-bsd-sockets:socket-close socket)
                                (bt2:destroy-thread thread)
                                (on-close-connection (transport-jsonrpc transport) connection))))
                          :name "jsonrpc/transport/local-domain-socket reading")
                         threads))
      (map () #'sb-thread:destroy-thread threads)
      (sb-bsd-sockets:socket-close listen-socket))))

(defmethod start-client ((transport local-domain-socket-transport))
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (sb-bsd-sockets:socket-connect socket (local-domain-socket-transport-address transport))
    (let* ((stream (sb-bsd-sockets:socket-make-stream socket
                                                      :input t
                                                      :output t
                                                      :buffering :full))
           (connection (make-instance 'connection
                                      :stream stream
                                      :request-callback (transport-message-callback transport))))
      (setf (transport-connection transport) connection)
      (on-open-connection (transport-jsonrpc transport) connection)
      (let* ((processing-loop-thread
               (bt2:make-thread
                (lambda ()
                  (run-processing-loop transport connection))
                :name "jsonrpc/transport/local-domain-socket processing"))
             (reading-loop-thread
               (bt2:make-thread
                (lambda ()
                  (unwind-protect (handler-case
                                      (run-reading-loop transport connection)
                                    (end-of-file ()))
                    (sb-bsd-sockets:socket-close socket)
                    (bt2:destroy-thread processing-loop-thread)))
                :name "jsonrpc/transport/local-domain-socket reading")))
        (setf (transport-threads transport)
              (list processing-loop-thread
                    reading-loop-thread)))
      connection)))

(defmethod send-message-using-transport
    ((transport local-domain-socket-transport) connection message)
  (write-message message (connection-stream connection)))

(defmethod receive-message-using-transport ((transport local-domain-socket-transport) connection)
  (read-message (connection-stream connection)))
