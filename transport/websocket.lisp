(in-package #:cl-user)
(defpackage #:jsonrpc/transport/websocket
  (:use #:cl
        #:jsonrpc/transport/interface
        #:jsonrpc/utils)
  (:import-from #:jsonrpc/connection
                #:connection
                #:connection-socket
                #:add-message-to-queue)
  (:import-from #:jsonrpc/request-response
                #:parse-message)
  (:import-from #:jsonrpc/errors
                #:jsonrpc-error)
  (:import-from #:bordeaux-threads
                #:make-thread
                #:destroy-thread)
  (:import-from #:event-emitter
                #:on
                #:emit)
  (:import-from #:yason)
  (:import-from #:quri)
  (:import-from #:websocket-driver)
  (:import-from #:clack)
  (:import-from #:clack.handler.hunchentoot)
  (:export #:websocket-transport
           #:make-clack-app))
(in-package #:jsonrpc/transport/websocket)

(defclass websocket-transport (transport)
  ((host :accessor websocket-transport-host
         :initarg :host
         :initform "127.0.0.1")
   (port :accessor websocket-transport-port
         :initarg :port
         :initform (random-port))
   (path :accessor websocket-transport-path
         :initarg :path
         :initform "/")
   (securep :accessor websocket-transport-secure-p
            :initarg :securep
            :initform nil)
   (debug :initarg :debug
          :initform t)))

(defmethod initialize-instance :after ((transport websocket-transport) &rest initargs &key url &allow-other-keys)
  (declare (ignore initargs))
  (when url
    (let ((uri (quri:uri url)))
      (unless (member (quri:uri-scheme uri) '("ws" "wss") :test #'equalp)
        (error "Only ws or wss are supported for websocket-transport (specified ~S)" (quri:uri-scheme uri)))
      (setf (websocket-transport-secure-p transport)
            (equalp (quri:uri-scheme uri) "wss"))
      (setf (websocket-transport-host transport) (quri:uri-host uri))
      (setf (websocket-transport-port transport) (quri:uri-port uri))
      (setf (websocket-transport-path transport) (quri:uri-path uri))))
  transport)


(defun make-clack-app (transport)
  (flet ((json-rpc-websocket-app (env)
           (block nil
             ;; Return 200 OK for non-WebSocket requests
             (unless (wsd:websocket-p env)
               (return '(200 () ("ok"))))
             (let* ((ws (wsd:make-server env))
                    (connection (make-instance 'connection
                                               :socket ws
                                               :request-callback
                                               (transport-message-callback transport))))

               (on :message ws
                   (lambda (input)
                     (let ((message (handler-case (parse-message input)
                                      (jsonrpc-error ()
                                        ;; Nothing can be done
                                        nil))))
                       (when message
                         (add-message-to-queue connection message)))))

               (on :open ws
                   (lambda ()
                     (emit :open transport connection)))

               (on :close ws
                   (lambda (&key code reason)
                     (declare (ignore code reason))
                     (emit :close connection)))

               (lambda (responder)
                 (declare (ignore responder))
                 (let ((thread
                         (bt2:make-thread
                          (lambda ()
                            (run-processing-loop transport connection))
                          :name "jsonrpc/transport/websocket processing")))
                   (unwind-protect
                        (wsd:start-connection ws)
                     (bt2:destroy-thread thread))))))))
    #'json-rpc-websocket-app))


(defmethod start-server ((transport websocket-transport))
  (setf (transport-connection transport)
        (clack:clackup
         (make-clack-app transport)
         :host (websocket-transport-host transport)
         :port (websocket-transport-port transport)
         :server :hunchentoot
         :debug (slot-value transport 'debug)
         :use-thread nil)))

(defmethod start-client ((transport websocket-transport))
  (let* ((client (wsd:make-client (format nil "~A://~A:~A~A"
                                          (if (websocket-transport-secure-p transport)
                                              "wss"
                                              "ws")
                                          (websocket-transport-host transport)
                                          (websocket-transport-port transport)
                                          (websocket-transport-path transport))))
         (connection (make-instance 'connection
                                    :socket client
                                    :request-callback
                                    (transport-message-callback transport))))
    (on :open client
        (lambda ()
          (emit :open transport connection)))

    (on :close client
        (lambda (&key code reason)
          (declare (ignore code reason))
          ;; Reconnect automatically
          (wsd:start-connection client)))

    (on :message client
        (lambda (input)
          (let ((message (parse-message input)))
            (when message
              (add-message-to-queue connection message)))))

    (wsd:start-connection client)
    (setf (transport-connection transport) connection)

    (setf (transport-threads transport)
          (list
           (bt2:make-thread
            (lambda ()
              (run-processing-loop transport connection))
            :name "jsonrpc/transport/websocket processing")
           ;; KLUDGE: Requires to kill the read-thread of WebSocket client
           ;;   for calling 'close-connection'.
           ;;   Perhaps, finalization should be done in other places.
           (slot-value client 'websocket-driver.ws.client::read-thread)))
    connection))

(defmethod send-message-using-transport ((transport websocket-transport) connection message)
  (let ((json (with-output-to-string (s)
                (yason:encode message s)))
        (ws (connection-socket connection)))
    (wsd:send ws json)))

(defmethod receive-message-using-transport ((transport websocket-transport) connection)
  (error "Not allowed to receive synchronously with WebSocket transport."))
