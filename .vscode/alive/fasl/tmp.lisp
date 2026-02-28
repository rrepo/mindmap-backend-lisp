(in-package :websocket-app)

;;; ---------------------------
;;; グローバル変数
;;; ---------------------------
(defvar *http-routes* (make-hash-table :test #'equal))
(defvar *ws-routes* (make-hash-table :test #'equal))
(defparameter *ws-clients* (make-hash-table :test 'eq)) ; ws -> client
(defparameter *subscriptions* (make-hash-table :test 'equal)) ; target -> (ws -> uuid)
(defparameter *ws-sessions* (make-hash-table :test 'equal)) ; token -> session
(defparameter *user-sessions* (make-hash-table :test 'equal)) ; uid -> token

;;; ---------------------------
;;; HTTP / WS ルーティング
;;; ---------------------------
(defvar *my-app*
        (lambda (env)
          (let* ((path (getf env :path-info))
                 (method (getf env :request-method)))
            (cond
             ;; CORS preflight
             ((string= method "OPTIONS")
               (list 200 '(:content-type "text/plain") '("OK")))

             ;; ---- WS は先に分岐 ----
             ((gethash path *ws-routes*)
               (funcall (gethash path *ws-routes*) env))

             ;; ---- それ以外の HTTP は token 必須 ----
             ((not (server-utils:validate-service-token env))
               (list 401 '(:content-type "text/plain") '("Unauthorized")))

             ((gethash path *http-routes*)
               (funcall (gethash path *http-routes*) env))

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
       (block ws-auth-block
         (let* ((ws (make-server env))
                (params (or (getf env :get-parameters)
                            (quri:url-decode-params
                             (getf env :query-string))))
                (ws-token (cdr (assoc "token" params :test #'string=)))
                (session (when ws-token
                               (gethash ws-token *ws-sessions*))))

           ;; ==========
           ;; 認証チェック
           ;; ==========
           (unless (and ws-token session)
             (format *error-output*
                 "[ws-auth] No valid session found for token: ~A~%"
               ws-token)
             (return-from ws-auth-block
                          '(401 (:content-type "text/plain") ("Unauthorized"))))

           (let* ((uid (getf session :uid))
                  (created-at (getf session :created-at)))

             (unless uid
               (websocket-driver:close-connection ws 1008 "Invalid session")
               (return-from ws-auth-block nil))

             (let ((age (- (get-universal-time) created-at)))
               (when (> age 500)
                     (remhash ws-token *ws-sessions*)
                     (remhash uid *user-sessions*)
                     (websocket-driver:close-connection ws 1008 "Session expired")
                     (return-from ws-auth-block nil)))

             (let ((user-token (gethash uid *user-sessions*)))
               (unless (and user-token
                            (string= ws-token user-token))
                 (websocket-driver:close-connection ws 1008 "Token mismatch")
                 (return-from ws-auth-block nil)))

             (format *error-output*
                 "[ws-auth] Authorized connection for uid ~A~%" uid))

           ;; 登録
           ; (setf (gethash ws *ws-clients*)
           ;   (list :ws ws
           ;         :subscriptions (make-hash-table :test 'equal)))

           ,@body

           (lambda (_responder)
             (websocket-driver:start-connection ws
                                                :on-close (lambda ()
                                                            (format t "[TEST] close from start-connection~%")
                                                            (controllers.ws:ws-on-close ws)))))))))

;;; ---------------------------
;;; WebSocket ハンドラ例
;;; ---------------------------
; (defroute-ws "/websocket"
;   (on :open ws
;       (lambda () (controllers.ws:ws-on-open ws)))
;   (on :message ws
;       (lambda (msg) (controllers.ws:ws-on-message ws msg)))
;   (on :close ws
;       (lambda () (controllers.ws:ws-on-close ws)))) ; ← これだけで管理

(defroute-ws "/websocket"
  (websocket-driver:on :open ws
                       (lambda ()
                         (controllers.ws:ws-on-open ws)))

  (websocket-driver:on :message ws
                       (lambda (msg)
                         (controllers.ws:ws-on-message ws msg)))

  (websocket-driver:on :close ws
                       (lambda ()
                         (controllers.ws:ws-on-close ws))))

(defroute-http "/ws-token"
  (server-utils:with-api-response(controllers.ws:handle-ws-token env)))

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
