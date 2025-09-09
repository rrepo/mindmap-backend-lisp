(defpackage :websocket-app
  (:use :cl :clack :websocket-driver)
  (:export :start-app :*my-app*))

(in-package :websocket-app)

(load "./controllers/users.lisp")
(load "./controllers/mindmaps.lisp")
(load "./utils/utils.lisp")

(defmacro defroute-http (path &body body)
  `(setf (gethash ,path *http-routes*)
     (lambda (env) ,@body)))

(defmacro defroute-ws (path &body body)
  `(setf (gethash ,path *ws-routes*)
     (lambda (env)
       (let ((ws (make-server env)))
         ,@body
         (lambda (responder)
           (declare (ignore responder))
           (start-connection ws))))))

(defvar *http-routes* (make-hash-table :test #'equal))
(defvar *ws-routes* (make-hash-table :test #'equal))

(defvar *my-app*
        (lambda (env)
          (let* ((path (getf env :path-info))
                 (headers (getf env :headers))
                 (upgrade (utils:header-value headers "upgrade"))
                 (connection (utils:header-value headers "connection")))
            ;; WebSocket
            (if (and (string= path "/websocket")
                     upgrade
                     (string-equal (string-downcase upgrade) "websocket")
                     connection
                     (search "upgrade" (string-downcase connection)))
                (let ((ws-handler (gethash path *ws-routes*)))
                  (if ws-handler
                      (funcall ws-handler env)
                      '(404 (:content-type "text/plain") ("Not Found"))))
                ;; HTTP
                (let ((handler (gethash path *http-routes*)))
                  (if handler
                      (funcall handler env)
                      '(404 (:content-type "text/plain") ("Not Found"))))))))

(defmacro with-api-response (result)
  `(let ((res ,result))
     (cond
      ((null res) ;; データなし
                 `(200 (:content-type "application/json")
                       (,(jonathan:to-json '(:status "success" :data ())))))
      ((eq res :invalid)
        `(400 (:content-type "application/json")
              (,(jonathan:to-json '(:status "error")))))
      (t
        `(200 (:content-type "application/json")
              (,(jonathan:to-json
                 (list :status "success" :data res))))))))


(defroute-http "/"
               '(200 (:content-type "text/plain") ("Hello from /")))

(defroute-http "/user"
               (with-api-response (controllers.users:get-user env)))

(defroute-http "/users"
               (with-api-response (controllers.users:get-users env)))

(defroute-http "/all-users"
               (with-api-response (controllers.users:get-all-users)))

(defroute-http "/create-user"
               (with-api-response (controllers.users:create-user env)))

(defroute-http "/update-user"
               (with-api-response (controllers.users:update-user env)))

(defroute-http "/delete-user"
               (with-api-response (controllers.users:delete-user env)))

(defroute-http "/get-map"
               (with-api-response (controllers.mindmaps:get-map env)))

(defroute-http "/all-maps"
               (with-api-response (controllers.mindmaps:get-all-maps)))

(defroute-http "/create-map"
               (with-api-response (controllers.mindmaps:create-map env)))

(defroute-http "/update-map"
               (with-api-response (controllers.mindmaps:update-map env)))

(defroute-http "/delete-map"
               (with-api-response (controllers.mindmaps:delete-map env)))

(defroute-ws "/websocket"
             (on :message ws
                 (lambda (msg)
                   (format t "~&[WS] Received: ~A~%" msg)
                   (send ws (concatenate 'string "Echo: " msg)))))

(defun start-app (&key (port 5000))
  (clack:clackup *my-app* :server :woo :port port))

; curl -X POST      -H "Content-Type: application/json"      -d '{"uid":"u123", "name":"Taro", "img":"http://example.com"}'      http://localhost:5000/create-user  