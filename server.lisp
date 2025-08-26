(ql:quickload '(:clack :woo :websocket-driver-server))
(use-package :websocket-driver)

(defun header-value (headers name)
  (gethash name headers))

(defparameter *http-routes* (make-hash-table :test #'equal))
(defparameter *ws-routes* (make-hash-table :test #'equal))

(defmacro defroute-http (path lambda-form)
  `(setf (gethash ,path *http-routes*) ,lambda-form))

(defmacro defroute-ws (path &body body)
  `(setf (gethash ,path *ws-routes*)
     (lambda (env)
       (let ((ws (make-server env)))
         ,@body
         (lambda (responder)
           (declare (ignore responder))
           (start-connection ws))))))

(defmacro defroute-http (path &body body)
  `(setf (gethash ,path *http-routes*)
     (lambda () ,@body)))

(defvar *my-app*
        (lambda (env)
          (let* ((path (getf env :path-info))
                 (headers (getf env :headers))
                 (upgrade (header-value headers "upgrade"))
                 (connection (header-value headers "connection")))

            ;; WebSocket
            (if (and (string= path "/websocket")
                     upgrade
                     (string-equal (string-downcase upgrade) "websocket")
                     connection
                     (search "upgrade" (string-downcase connection)))
                (let ((ws-handler (gethash path *ws-routes*)))
                  (if ws-handler
                      (funcall ws-handler env)
                      ;; fallback if no ws handler
                      '(404 (:content-type "text/plain") ("Not Found"))))
                ;; otherwise HTTP
                (let ((handler (gethash path *http-routes*)))
                  (if handler
                      (funcall handler)
                      '(404 (:content-type "text/plain") ("Not Found"))))))))

(defroute-http "/"
               '(200 (:content-type "text/plain") ("Hello from /")))

(defroute-ws "/websocket"
             (on :message ws
                 (lambda (msg)
                   (format t "~&[WS] Received: ~A~%" msg)
                   (send ws (concatenate 'string "Echo: " msg)))))

(clack:clackup *my-app* :server :woo :port 5000)
