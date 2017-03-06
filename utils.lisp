(in-package #:cl-user)
(defpackage #:jsonrpc/utils
  (:use #:cl)
  (:import-from #:usocket
                #:socket-listen
                #:socket-close
                #:address-in-use-error)
  (:import-from #:bordeaux-threads
                #:make-lock
                #:with-lock-held)
  (:export #:random-port
           #:make-id
           #:find-mode-class))
(in-package #:jsonrpc/utils)

(defun port-available-p (port)
  (handler-case (let ((socket (usocket:socket-listen "127.0.0.1" port :reuse-address t)))
                  (usocket:socket-close socket))
    (usocket:address-in-use-error (e) (declare (ignore e)) nil)))

(defun random-port ()
  "Return a port number not in use from 50000 to 60000."
  (loop for port from (+ 50000 (random 1000)) upto 60000
        if (port-available-p port)
          return port))

(defun make-id (&optional (length 12))
  (declare (type fixnum length))
  (let ((result (make-string length)))
    (declare (type simple-string result))
    (dotimes (i length result)
      (setf (aref result i)
            (ecase (random 5)
              ((0 1) (code-char (+ #.(char-code #\a) (random 26))))
              ((2 3) (code-char (+ #.(char-code #\A) (random 26))))
              ((4) (code-char (+ #.(char-code #\0) (random 10)))))))))

(defvar *transport-load-lock* (bt:make-lock))
(defun find-mode-class (mode)
  (let ((system-name (format nil "jsonrpc/transport/~(~A~)" mode))
        (package-name (format nil "~A/~A"
                              :jsonrpc/transport
                              mode)))
    (when (asdf:find-system system-name nil)
      (bt:with-lock-held (*transport-load-lock*)
        #+quicklisp
        (ql:quickload system-name :silent t)
        #-quicklisp
        (asdf:load-system system-name :verbose nil))
      (let ((package (find-package package-name)))
        (and package
             (find-class (intern (format nil "~A-~A" mode :transport) package)))))))
