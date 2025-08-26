(defpackage :websocket-app
  (:use :cl :clack :websocket-driver)
  (:export :start-app))

(in-package :websocket-app)

(load "./controllers/users.lisp")
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

(defun header-value (headers name)
  (gethash name headers))

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
                      '(404 (:content-type "text/plain") ("Not Found"))))
                ;; HTTP
                (let ((handler (gethash path *http-routes*)))
                  (if handler
                      (funcall handler env)
                      '(404 (:content-type "text/plain") ("Not Found"))))))))

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
                      '(404 (:content-type "text/plain") ("Not Found"))))
                ;; HTTP
                (let ((handler (gethash path *http-routes*)))
                  (if handler
                      (funcall handler env)
                      '(404 (:content-type "text/plain") ("Not Found"))))))))

(defroute-http "/"
               '(200 (:content-type "text/plain") ("Hello from /")))

(defroute-http "/users"
               `(200 (:content-type "application/json") ,(list (controllers.users:get-users))))

(defroute-http "/user"
               (let* ((qs (getf env :query-string))
                      (params (utils:parse-query-string qs))
                      (ht (alexandria:alist-hash-table params :test 'equal))
                      (json-string (jonathan:to-json ht))
                      (user (controllers.users:get-user json-string)))
                 (destructuring-bind (uid name img) (first user)
                   (let ((user-plist `(:uid ,uid :name ,name :img ,img)))
                     `(200 (:content-type "application/json")
                           (list ,(jonathan:to-json user-plist))))))
                           
                           
                           
                           )

(defroute-http "/create-user"
               (let* ((headers (getf env :headers))
                      (content-length (parse-integer (or (header-value headers "content-length") "0")
                                                     :junk-allowed t))
                      (input (getf env :raw-body))
                      (body-string (utils:parse-request-body-string input content-length))
                      (result (controllers.users:create-user body-string)))
                 (cond
                  ((eq result :success)
                    `(200 (:content-type "application/json")
                          (,(jonathan:to-json '(("status" . "success"))))))
                  (t
                    `(500 (:content-type "application/json")
                          ("{\"status\":\"error\"}"))))))


(defroute-ws "/websocket"
             (on :message ws
                 (lambda (msg)
                   (format t "~&[WS] Received: ~A~%" msg)
                   (send ws (concatenate 'string "Echo: " msg)))))

(defun start-app (&key (port 5000))
  (clack:clackup *my-app* :server :woo :port port))

; curl -X POST      -H "Content-Type: application/json"      -d '{"uid":"u123", "name":"Taro", "img":"http://example.com"}'      http://localhost:5000/create-user  