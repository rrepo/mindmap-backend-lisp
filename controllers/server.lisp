(defpackage :websocket-app
  (:use :cl :clack :websocket-driver :cl-dotenv)
  (:export :start-app :*my-app*))

(in-package :websocket-app)

(load "./controllers/users.lisp")
(load "./controllers/maps.lisp")
(load "./controllers/nodes.lisp")
(load "./controllers/map-members.lisp")
(load "./controllers/map-invitations.lisp")
(load "./services/mindmaps.lisp")
(load "./utils/utils.lisp")
(load "./utils/env.lisp")

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

(defun with-cors (handler)
  (lambda (env)
    (multiple-value-bind (status headers body)
        (funcall handler env)
      (values status
        (append
          '(("Access-Control-Allow-Origin" . "http://localhost:3000/")
            ("Access-Control-Allow-Methods" . "GET, POST, PUT, DELETE, OPTIONS")
            ("Access-Control-Allow-Headers" . "Content-Type, Authorization"))
          headers)
        body))))

(defun validate-service-token (env)
  "env から X-Service-Token を取り出し、*backend-token-secret* と一致するか検証する。
ヘッダがハッシュテーブル／plist／alist のいずれでも動作するようにする。"
  (let* ((headers (getf env :headers))
         (find-in-htable
          (lambda (ht key)
            (let ((found nil))
              (maphash (lambda (k v)
                         (when (and (stringp (string k))
                                    (string= (string-downcase (string k))
                                             (string-downcase key)))
                               (setf found v)))
                       ht)
              found)))
         (token
          (cond
           ((typep headers 'hash-table)
             (funcall find-in-htable headers "x-service-token"))
           ((listp headers)
             (or (getf headers :x-service-token)
                 (getf headers :X-SERVICE-TOKEN)
                 (let ((pair (assoc "x-service-token" headers :test
                                    (lambda (a b)
                                      (and (stringp a)
                                           (string= (string-downcase a)
                                                    (string-downcase b)))))))
                   (when pair (cdr pair)))))
           (t nil))))
    (and token (string= token utils-env:*backend-token-secret*))))


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
             ((not (validate-service-token env))
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
               (with-api-response (controllers.maps:get-map env)))

(defroute-http "/all-maps"
               (with-api-response (controllers.maps:get-all-maps)))

(defroute-http "/create-map"
               (with-api-response (controllers.maps:create-map env)))

(defroute-http "/update-map"
               (with-api-response (controllers.maps:update-map env)))

(defroute-http "/delete-map"
               (with-api-response (controllers.maps:delete-map env)))

(defroute-http "/all-nodes"
               (with-api-response (controllers.nodes:get-all-nodes)))

(defroute-http "/create-node"
               (with-api-response (controllers.nodes:create-node env)))

(defroute-http "/update-node"
               (with-api-response (controllers.nodes:update-node env)))

(defroute-http "/delete-node"
               (with-api-response (controllers.nodes:delete-node env)))

(defroute-http "/get-map-member"
               (with-api-response (controllers.map-members:get-map-member env)))

(defroute-http "/get-map-members-by-map-id"
               (with-api-response (controllers.map-members:get-map-members-by-map-id env)))

(defroute-http "/get-map-members-by-user-uid"
               (with-api-response (controllers.map-members:get-map-members-by-user-uid env)))

(defroute-http "/all-map-members"
               (with-api-response (controllers.map-members:get-all-map-members)))

(defroute-http "/create-map-member"
               (with-api-response (controllers.map-members:create-map-member env)))

(defroute-http "/delete-map-member"
               (with-api-response (controllers.map-members:delete-map-member env)))

(defroute-http "/get-map-invitation"
               (with-api-response (controllers.map-invitations:get-map-invitation env)))

(defroute-http "/get-map-invitation-by-token"
               (with-api-response (controllers.map-invitations:get-map-invitation-by-token env)))

(defroute-http "/get-map-invitation-by-map-id"
               (with-api-response (controllers.map-invitations:get-map-invitation-by-map-id env)))

(defroute-http "/create-map-invitation"
               (with-api-response (controllers.map-invitations:create-map-invitation env)))

(defroute-http "/delete-map-invitation"
               (with-api-response (controllers.map-invitations:delete-map-invitation env)))

(defroute-http "/get-map-detiels"
               (with-api-response (controllers.maps:get-map-detiels env)))

(defroute-ws "/websocket"
             (on :message ws
                 (lambda (msg)
                   (format t "~&[WS] Received: ~A~%" msg)
                   (send ws (concatenate 'string "Echo: " msg)))))

(defun start-app (&key (port 5000))
  (clack:clackup *my-app* :server :woo :port port))

; curl -X POST      -H "Content-Type: application/json"      -d '{"uid":"u123", "name":"Taro", "img":"http://example.com"}'      http://localhost:5000/create-user