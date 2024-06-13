;; TODO(cxxxr): このパッケージを削除する

(defpackage #:jsonrpc/class
  (:use #:cl)
  (:use #:jsonrpc/base)
  (:import-from #:jsonrpc/base
                #:jsonrpc
                #:jsonrpc-transport
                #:ensure-connected)
  (:import-from #:jsonrpc/mapper
                #:exposable
                #:expose
                #:register-method
                #:clear-methods
                #:dispatch)
  (:import-from #:jsonrpc/transport/interface
                #:transport-message-callback
                #:transport-jsonrpc
                #:transport
                #:transport-connection
                #:transport-threads
                #:start-server
                #:start-client
                #:receive-message-using-transport)
  (:import-from #:jsonrpc/connection
                #:connection
                #:*connection*
                #:set-callback-for-id
                #:add-message-to-outbox)
  (:import-from #:jsonrpc/request-response
                #:make-request
		#:jsonrpc-version
		#:*jsonrpc-version*
                #:response-error
                #:response-error-code
                #:response-error-message
                #:response-result)
  (:import-from #:jsonrpc/errors
                #:jsonrpc-callback-error)
  (:import-from #:jsonrpc/utils
                #:hash-exists-p
                #:find-mode-class
                #:make-id)
  (:import-from #:bordeaux-threads
                #:*default-special-bindings*
                #:destroy-thread)
  (:import-from #:alexandria
                #:deletef
                #:remove-from-plist)
  )
(in-package #:jsonrpc/class)
