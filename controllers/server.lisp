(defpackage :websocket-app
  (:use :cl :clack :websocket-driver :cl-dotenv)
  (:export :start-app :*my-app* :stop-app))

(in-package :websocket-app)

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
        ; (with-cors
        (lambda (env)
          (let* ((path (getf env :path-info))
                 (method (getf env :request-method)))
            (cond
             ;; OPTIONSリクエスト (CORS preflight)
             ((string= method "OPTIONS")
               (list 200
                     '(:content-type "text/plain")
                     '("OK")))

             ;; トークン認証（有効）
             ((not (server-utils:validate-service-token env))
               (list 401
                     '(:content-type "text/plain")
                     '("Unauthorized")))

             ;; HTTPルート
             ((gethash path *http-routes*)
               (funcall (gethash path *http-routes*) env))

             ;; WSルート
             ((gethash path *ws-routes*)
               (funcall (gethash path *ws-routes*) env))

             ;; Not Found
             (t
               (list 404
                     '(:content-type "text/plain")
                     '("Not Found")))))))

(defroute-http "/"
               '(200 (:content-type "text/plain") ("Hello from /5t")))

(defroute-http "/login"
               (server-utils:with-api-response (controllers.users:handle-login env)))

(defroute-http "/user"
               (server-utils:with-api-response (controllers.users:handle-get-user env)))

(defroute-http "/users"
               (server-utils:with-api-response (controllers.users:handle-get-users env)))

(defroute-http "/all-users"
               (server-utils:with-api-response (controllers.users:handle-get-all-users)))

(defroute-http "/create-user"
               (server-utils:with-api-response (controllers.users:handle-create-user env)))

(defroute-http "/update-user"
               (server-utils:with-api-response (controllers.users:handle-update-user env)))

(defroute-http "/delete-user"
               (server-utils:with-api-response (controllers.users:handle-delete-user env)))

(defroute-http "/get-map"
               (server-utils:with-api-response (controllers.maps:handle-get-map env)))

(defroute-http "/all-maps"
               (server-utils:with-api-response (controllers.maps:handle-get-all-maps)))

(defroute-http "/create-map"
               (server-utils:with-api-response (controllers.maps:handle-create-map env)))

(defroute-http "/update-map"
               (server-utils:with-api-response (controllers.maps:handle-update-map env)))

(defroute-http "/delete-map"
               (server-utils:with-api-response (controllers.maps:handle-delete-map env)))

(defroute-http "/all-nodes"
               (server-utils:with-api-response (controllers.nodes:handle-get-all-nodes)))

(defroute-http "/create-node"
               (server-utils:with-api-response (controllers.nodes:handle-create-node env)))

(defroute-http "/update-node"
               (server-utils:with-api-response (controllers.nodes:handle-update-node env)))

(defroute-http "/delete-node"
               (server-utils:with-api-response (controllers.nodes:handle-delete-node env)))

(defroute-http "/get-map-member"
               (server-utils:with-api-response (controllers.map-members:handle-get-map-member env)))

(defroute-http "/get-map-members-by-map-id"
               (server-utils:with-api-response (controllers.map-members:handle-get-map-members-by-map-id env)))

(defroute-http "/get-map-members-by-user-uid"
               (server-utils:with-api-response (controllers.map-members:handle-get-map-members-by-user-uid env)))

(defroute-http "/all-map-members"
               (server-utils:with-api-response (controllers.map-members:handle-get-all-map-members)))

(defroute-http "/create-map-member"
               (server-utils:with-api-response (controllers.map-members:handle-create-map-member env)))

(defroute-http "/delete-map-member"
               (server-utils:with-api-response (controllers.map-members:handle-delete-map-member env)))

(defroute-http "/get-map-invitation"
               (server-utils:with-api-response (controllers.map-invitations:handle-get-map-invitation env)))

(defroute-http "/get-map-invitation-by-token"
               (server-utils:with-api-response (controllers.map-invitations:handle-get-map-invitation-by-token env)))

(defroute-http "/get-map-invitation-by-map-id"
               (server-utils:with-api-response (controllers.map-invitations:handle-get-map-invitation-by-map-id env)))

(defroute-http "/create-map-invitation"
               (server-utils:with-api-response (controllers.map-invitations:handle-create-map-invitation env)))

(defroute-http "/delete-map-invitation"
               (server-utils:with-api-response (controllers.map-invitations:handle-delete-map-invitation env)))

(defroute-http "/get-map-details"
               (server-utils:with-api-response (controllers.maps:handle-get-map-details env)))

(defroute-ws "/websocket"
             (on :message ws
                 (lambda (msg)
                   (format t "~&[WS] Received: ~A~%" msg)
                   (send ws (concatenate 'string "Echo: " msg)))))


(defvar *current-server* nil
        "現在起動中の Mindmap サーバーを保持。")

(defun start-app (&key (port 5000))
  "サーバーを起動してサーバーオブジェクトを返す。"
  ;; すでにサーバーがあれば停止してから起動
  (when *current-server*
        (stop-app))
  (format t "Starting Mindmap server on port ~a...~%" port)
  ;; Clack のサーバーを起動してオブジェクトを保存
  (setf *current-server* (clack:clackup *my-app* :server :woo :port port))
  *current-server*)

(defun stop-app ()
  "現在のサーバーを停止する。"
  (when *current-server*
        (format t "Stopping Mindmap server...~%")
        (ignore-errors
          ;; Clack/Woo 停止関数
          (clack:stop *current-server*))
        (setf *current-server* nil)))
