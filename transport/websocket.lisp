(defpackage #:jsonrpc/transport/websocket
  (:use #:cl
        #:jsonrpc/transport/interface
        #:jsonrpc/utils)
  (:import-from #:jsonrpc/base
                #:on-open-connection
                #:on-close-connection)
  (:import-from #:jsonrpc/connection
                #:connection
                #:connection-stream
                #:add-message-to-queue)
  (:import-from #:jsonrpc/request-response
                #:parse-message)
  (:import-from #:jsonrpc/errors
                #:jsonrpc-error)
  (:import-from #:bordeaux-threads
                #:make-thread
                #:destroy-thread)
  (:import-from #:event-emitter
                #:on)
  (:import-from #:yason)
  (:import-from #:quri)
  (:import-from #:websocket-driver)
  (:import-from #:clack)
  (:import-from #:clack.handler.hunchentoot)
  (:export #:websocket-transport
           #:make-clack-app))
(in-package #:jsonrpc/transport/websocket)

(defvar *clack-handler* 'default-handler)

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
   (clack-handler :accessor websocket-clack-handler
                  :initarg :clack-handler
                  :initform *clack-handler*)
   (debug :initarg :debug
          :initform t
          :reader websocket-transport-debug-p)))

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


(defun default-handler (env)
  ;; Return 200 OK for non-WebSocket requests
  (unless (wsd:websocket-p env)
    '(200 () ("ok"))))

(defun make-clack-app (transport)
  (flet ((json-rpc-websocket-app (env)
           (block nil
             (let ((result (funcall (websocket-clack-handler transport) env)))
               (when result
                 (return result)))
             (let* ((ws (wsd:make-server env))
                    (connection (make-instance 'connection
                                               :stream ws
                                               :request-callback
                                               (transport-message-callback transport))))

               (setf (transport-connection transport) connection)

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
                     (on-open-connection (transport-jsonrpc transport) connection)))

               (on :close ws
                   (lambda (&key code reason)
                     (declare (ignore code reason))
                     (on-close-connection (transport-jsonrpc transport) connection)))

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
         :address (websocket-transport-host transport)
         :port (websocket-transport-port transport)
         :server :hunchentoot
         :debug (websocket-transport-debug-p transport)
         :use-thread nil)))

(defmethod start-client ((transport websocket-transport))
  (let* ((client (wsd:make-client (quri:render-uri
				   (quri:make-uri
				    :scheme (if (websocket-transport-secure-p transport)
						"wss"
						"ws")
				    :host (websocket-transport-host transport)
				    :port (websocket-transport-port transport)
				    :path (websocket-transport-path transport)))))
         (connection (make-instance 'connection
                                    :stream client
                                    :request-callback
                                    (transport-message-callback transport))))
    (on :open client
        (lambda ()
          (on-open-connection (transport-jsonrpc transport) connection)))

    (on :close client
        (lambda (&key code reason)
          (declare (ignore code reason))
          ;; Reconnect automatically
          (wsd:start-connection client)))

    (on :message client
        (lambda (input)
          (let ((message (handler-case (parse-message input)
                           (jsonrpc-error ()
                             ;; Nothing can be done
                             nil))))
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
        (ws (connection-stream connection)))
    (wsd:send ws json)))

(defmethod receive-message-using-transport ((transport websocket-transport) connection)
  (error "Not allowed to receive synchronously with WebSocket transport."))
