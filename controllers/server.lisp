(in-package :websocket-app)

;;; ---------------------------
;;; グローバル変数
;;; ---------------------------
(defvar *http-routes* (make-hash-table :test #'equal))
(defvar *ws-routes* (make-hash-table :test #'equal))
(defparameter *ws-clients* (make-hash-table :test 'eq)) ; ws -> client
(defparameter *subscriptions* (make-hash-table :test 'equal)) ; target -> (ws -> uuid)

;;; ---------------------------
;;; HTTP / WS ルーティング
;;; ---------------------------
(defvar *my-app*
        (lambda (env)
          (let* ((path (getf env :path-info))
                 (method (getf env :request-method)))
            (cond
             ((string= method "OPTIONS")
               (list 200 '(:content-type "text/plain") '("OK")))
             ((not (server-utils:validate-service-token env))
               (list 401 '(:content-type "text/plain") '("Unauthorized")))
             ((gethash path *http-routes*)
               (funcall (gethash path *http-routes*) env))
             ((gethash path *ws-routes*)
               (funcall (gethash path *ws-routes*) env))
             (t
               (list 404 '(:content-type "text/plain") '("Not Found")))))))

;;; ---------------------------
;;; HTTP / WS ルート定義マクロ
;;; ---------------------------
(defmacro defroute-http (path &body body)
  `(setf (gethash ,path *http-routes*)
     (lambda (env) ,@body)))

(defmacro defroute-ws (path &body body)
  `(setf (gethash ,path *ws-routes*)
     (lambda (env)
       (let* ((ws (make-server env))
              (client (list
                       :ws ws
                       :subscriptions (make-hash-table :test 'equal))))
         ;; ws -> client 登録
         (setf (gethash ws *ws-clients*) client)

         ;; close ハンドラ
         (on :close ws
             (lambda ()
               (ws-close-handler ws)))

         ;; ユーザーハンドラ
         ,@body

         ;; start connection
         (lambda (_responder)
           (start-connection ws))))))

;;; ---------------------------
;;; close handler
;;; ---------------------------
(defun ws-close-handler (ws)
  "WS が閉じたとき client を削除し、subscriptions からも取り除く"
  (let ((client (gethash ws *ws-clients*)))
    (remhash ws *ws-clients*)
    (maphash
      (lambda (_target bucket)
        (remhash ws bucket)
        (when (zerop (hash-table-count bucket))
              (remhash _target *subscriptions*)))
      *subscriptions*)
    (format t "[WS] Closed: ~A~%" ws)))

;;; ---------------------------
;;; SUBSCRIBE / UNSUBSCRIBE / BROADCAST
;;; ---------------------------
(defun ws-subscribe (ws target uuid)
  "WS を target の bucket に登録し client に subscriptions を記録"
  (let* ((client (gethash ws *ws-clients*))
         (subs (getf client :subscriptions))
         (bucket (gethash target *subscriptions*)))
    (unless subs
      (setf subs (make-hash-table :test 'equal))
      (setf (getf client :subscriptions) subs))
    (unless bucket
      (setf bucket (make-hash-table :test 'eq))
      (setf (gethash target *subscriptions*) bucket))
    (setf (gethash ws bucket) uuid)
    (setf (gethash target subs) uuid)
    ;; SUBSCRIBED メッセージ
    (send ws (jonathan:to-json
              `(:type "SUBSCRIBED"
                      :target ,target
                      :uuid ,uuid)))))

(defun ws-unsubscribe (ws target)
  "WS を target の bucket から削除し client subscriptions も削除"
  (let* ((client (gethash ws *ws-clients*))
         (subs (getf client :subscriptions))
         (bucket (gethash target *subscriptions*)))
    (when bucket
          (remhash ws bucket)
          (when (zerop (hash-table-count bucket))
                (remhash target *subscriptions*)))
    (when subs
          (remhash target subs))
    ;; UNSUBSCRIBED メッセージ
    (send ws (jonathan:to-json
              `(:type "UNSUBSCRIBED"
                      :target ,target)))))

(defun ws-broadcast-to-target (target message)
  "target に登録されているクライアントに broadcast"
  (let ((bucket (gethash target *subscriptions*)))
    (when bucket
          (maphash
            (lambda (ws _uuid)
              (ignore-errors
                (send ws message)))
            bucket))))

(defun ws-broadcast (message)
  "全クライアントに broadcast"
  (maphash
    (lambda (_ ws-client)
      (ignore-errors
        (send (getf ws-client :ws) message)))
    *ws-clients*))

;;; ---------------------------
;;; WebSocket ハンドラ例
;;; ---------------------------
(defroute-ws "/websocket"
             (on :message ws
                 (lambda (msg)
                   (handler-case
                       (let* ((data (jonathan:parse msg :as :hash-table))
                              (type (string-upcase (gethash "type" data)))
                              (target (gethash "target" data))
                              (uuid (gethash "uuid" data)))
                         (cond
                          ((string= type "SUBSCRIBE")
                            (format t "[WS SUB] target=~A uuid=~A~%" target uuid)
                            (ws-subscribe ws target uuid))

                          ((string= type "UNSUBSCRIBE")
                            (format t "[WS UNSUB] target=~A~%" target)
                            (ws-unsubscribe ws target))))

                     (error (e)
                       (format *error-output* "[WS ERROR] ~A~%" e))))))


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

(defroute-http "/get-maps-by-uid"
               (server-utils:with-api-response (controllers.maps:handle-get-maps-by-uid env)))

(defroute-http "/create-map"
               (server-utils:with-api-response (controllers.maps:handle-create-map env)))

(defroute-http "/update-map"
               (server-utils:with-api-response (controllers.maps:handle-update-map env)))

(defroute-http "/delete-map"
               (server-utils:with-api-response (controllers.maps:handle-delete-map env)))

(defroute-http "/count-private-maps"
               (server-utils:with-api-response (controllers.maps:handle-count-private-maps env)))

(defroute-http "/search-public-maps"
               (server-utils:with-api-response (controllers.maps:handle-get-public-maps-by-search env)))

(defroute-http "/get-latest-public-maps"
               (server-utils:with-api-response (controllers.maps:handle-get-public-maps env)))

(defroute-http "/all-nodes"
               (server-utils:with-api-response (controllers.nodes:handle-get-all-nodes)))

(defroute-http "/create-node"
               (server-utils:with-api-response (controllers.nodes:handle-create-node env)))

(defroute-http "/update-node"
               (server-utils:with-api-response (controllers.nodes:handle-update-node env)))

(defroute-http "/delete-node"
               (server-utils:with-api-response (controllers.nodes:handle-delete-node env)))

(defroute-http "/delete-node-descendants"
               (server-utils:with-api-response (controllers.nodes:handle-delete-node-descendants env)))

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

(defroute-http "/get-map-invitation-by-map-uuid"
               (server-utils:with-api-response (controllers.map-invitations:handle-get-map-invitation-by-map-uuid env)))

(defroute-http "/create-map-invitation"
               (server-utils:with-api-response (controllers.map-invitations:handle-create-map-invitation env)))

(defroute-http "/delete-map-invitation"
               (server-utils:with-api-response (controllers.map-invitations:handle-delete-map-invitation env)))

(defroute-http "/get-map-details"
               (server-utils:with-api-response (controllers.maps:handle-get-map-details env)))


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
