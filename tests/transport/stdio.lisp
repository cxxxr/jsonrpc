(in-package #:cl-user)
(defpackage #:jsonrpc/tests/transport/stdio
  (:use #:cl
        #:rove
        #:jsonrpc)
  (:import-from #:bordeaux-threads))
(in-package #:jsonrpc/tests/transport/stdio)

#+(and sbcl unix)
(deftest stdio-server
  (multiple-value-bind (inputfd-1 outputfd-1)
      (sb-posix:pipe)
    (multiple-value-bind (inputfd-2 outputfd-2)
        (sb-posix:pipe)
      (let ((server-thread
              (bt:make-thread
               (lambda ()
                 (let ((server (jsonrpc:make-server)))
                   (jsonrpc:expose server "sum" (lambda (args) (reduce #'+ args)))
                   (jsonrpc:server-listen server
                                          :mode :stdio
                                          :input (sb-sys:make-fd-stream inputfd-1 :input t)
                                          :output (sb-sys:make-fd-stream outputfd-2 :output t))))))
            (client (jsonrpc:make-client)))
        (unwind-protect
             (progn
               (sleep 0.5)
               (jsonrpc:client-connect client
                                       :mode :stdio
                                       :input (sb-sys:make-fd-stream inputfd-2 :input t)
                                       :output (sb-sys:make-fd-stream outputfd-1 :output t))
               (ok (= (jsonrpc:call client "sum" '(10 20)) 30)))
          (bt:destroy-thread server-thread)
          (jsonrpc:client-disconnect client))))))
