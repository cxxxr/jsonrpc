(in-package #:cl-user)
(defpackage #:jsonrpc/transport/websocket
  (:use #:cl
        #:jsonrpc/transport/interface
        #:jsonrpc/utils)
  (:import-from #:jsonrpc/request-response
                #:parse-message)
  (:import-from #:jsonrpc/errors
                #:jsonrpc-error)
  (:import-from #:bordeaux-threads)
  (:import-from #:yason)
  (:import-from #:quri)
  (:import-from #:websocket-driver)
  (:import-from #:clack)
  (:export #:websocket-transport))
(in-package #:jsonrpc/transport/websocket)

(defclass websocket-transport (transport)
  ((host :accessor websocket-transport-host
         :initarg :host
         :initform "127.0.0.1")
   (port :accessor websocket-transport-port
         :initarg :port
         :initform (random-port))
   (securep :accessor websocket-transport-secure-p
            :initarg :securep)
   (server :accessor websocket-transport-server
           :initarg :server
           :initform :hunchentoot)
   (debug :initarg :debug
          :initform t)
   (connect-cb :initarg :connect-cb
               :type (or null function)
               :initform nil)
   (client-queue :initform (make-array 0 :adjustable t :fill-pointer 0)
                 :accessor client-queue)
   (client-queue-lock :initform (bt:make-lock)
                      :accessor client-queue-lock)))

(defmethod initialize-instance :after ((transport websocket-transport) &rest initargs &key url &allow-other-keys)
  (declare (ignore initargs))
  (when url
    (let ((uri (quri:uri url)))
      (unless (member (quri:uri-scheme uri) '("ws" "wss") :test #'equalp)
        (error "Only ws or wss are supported for websocket-transport (specified ~S)" (quri:uri-scheme uri)))
      (setf (websocket-transport-secure-p transport)
            (equalp (quri:uri-scheme uri) "wss"))
      (setf (websocket-transport-host transport) (quri:uri-host uri))
      (setf (websocket-transport-port transport) (quri:uri-port uri))))
  transport)

(defmethod start-server ((transport websocket-transport))
  (clack:clackup
   (lambda (env)
     (let ((ws (wsd:make-server env)))
       (wsd:on :message ws
               (lambda (input)
                 (let ((message (handler-case (parse-message input)
                                  (jsonrpc-error ()
                                    ;; Nothing can be done
                                    nil))))
                   (when message
                     (handle-request-message transport ws message)))))
       (when (slot-value transport 'connect-cb)
         (wsd:on :open ws
                 (lambda ()
                   (funcall (slot-value transport 'connect-cb) ws))))
       (setf (transport-connection transport) ws)
       (lambda (responder)
         (declare (ignore responder))
         (wsd:start-connection ws))))
   :host (websocket-transport-host transport)
   :port (websocket-transport-port transport)
   :server (websocket-transport-server transport)
   :debug (slot-value transport 'debug)
   :use-thread nil))

(defmethod start-client ((transport websocket-transport))
  (let ((client (wsd:make-client (format nil "~A://~A:~A/"
                                         (if (websocket-transport-secure-p transport)
                                             "wss"
                                             "ws")
                                         (websocket-transport-host transport)
                                         (websocket-transport-port transport)))))
    (wsd:start-connection client)
    (setf (transport-connection transport) client)
    (when (slot-value transport 'connect-cb)
      (funcall (slot-value transport 'connect-cb) client))
    (wsd:on :message client
            (lambda (message)
              (bt:with-lock-held ((client-queue-lock transport))
                (vector-push-extend message (client-queue transport)))))
    transport))

(defmethod send-message-using-transport ((transport websocket-transport) ws message)
  (let ((json (with-output-to-string (s)
                (yason:encode message s))))
    (wsd:send ws json)))

(defmethod receive-message-using-transport ((transport websocket-transport) connection)
  (loop while (= (length (client-queue transport)) 0)
        do (sleep 0.1))
  (parse-message
   (bt:with-lock-held ((client-queue-lock transport))
     (vector-pop (client-queue transport)))))