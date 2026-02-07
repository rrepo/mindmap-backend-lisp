(in-package :controllers.ws)

(defun handle-ws-token-http-cookie (env)
  (let* ((token utils-env:*ws-token-secret*)
         (cookie (format nil
                     "ws-token=~a; HttpOnly; Path=/; SameSite=Lax"
                   token))
         (body (babel:string-to-octets "OK" :encoding :utf-8)))
    ;; logs
    (format t "[ws-auth] issue ws-token cookie~%")
    (format t "[ws-auth] token=~a~%" token)
    (format t "[ws-auth] Set-Cookie=~a~%" cookie)
    (finish-output)

    `(200
      (:headers (("Content-Type" . "text/plain")
                 ("Set-Cookie" . ,cookie)))
      ,body)))


; (let ((cookie (format nil
;   "ws-token=~a; HttpOnly; Secure; Path=/; SameSite=Lax"
;   token)))

(defun ws-on-open (ws)
  (setf (gethash ws websocket-app:*ws-clients*)
    (list :ws ws
          :subscriptions (make-hash-table :test 'equal)))
  (format t "[WS] OPEN: ~A~%" ws))


(defun ws-on-message (ws msg)
  (format t "[WS] RAW MESSAGE: ~A~%" msg)
  (handler-case
      (let* ((text (if (stringp msg)
                       msg
                       (babel:octets-to-string msg)))
             (data (jonathan:parse text :as :hash-table))
             (type (string-upcase (gethash "type" data))))
        (dispatch-ws-message ws type data))
    (error (e)
      (format *error-output* "[WS ERROR] ~A~%" e))))


(defun dispatch-ws-message (ws type data)
  (cond
   ((string= type "SUBSCRIBE")
     (handle-ws-subscribe ws data))
   ((string= type "UNSUBSCRIBE")
     (handle-ws-unsubscribe ws data))
   ((string= type "NODE-UPDATE")
     (handle-ws-node-update ws data))
   ((string= type "NODE-CREATE")
     (handle-ws-node-create ws data))
   ((string= type "NODE-DELETE")
     (handle-ws-node-delete ws data))
   (t
     (format *error-output* "[WS] Unknown type: ~A~%" type))))

(defun handle-ws-subscribe (ws data)
  (let ((target (gethash "target" data)))
    (when target
          (ws-utils:ws-subscribe ws target))))

(defun handle-ws-unsubscribe (ws data)
  (let ((target (gethash "target" data)))
    (when target
          (ws-utils:ws-unsubscribe ws target))))

(defun handle-ws-node-create (ws data)
  (let* ((map-uuid (gethash "map-uuid" data))
         (map-id (gethash "map-id" data))
         (parent-id (gethash "parent-id" data))
         (uid (gethash "uid" data))
         (content (gethash "content" data))
         (client-id (gethash "client-id" data)))

    (format *error-output*
        "[WS] Create params: map-id=~A, map-uuid=~A, parent-id=~A, uid=~A, content=~A, client-id=~A~%"
      map-id map-uuid parent-id uid content client-id)

    (when (and map-id map-uuid content uid)
          ;; ① DB create
          (let ((node-id (models.nodes:create-node map-id parent-id content uid)))
            ;; ② WS broadcast
            (ws-utils:ws-broadcast-to-target
             (format nil "map-~A" map-uuid)
             (jonathan:to-json
              `(:type "NODE-CREATED"
                      :node-Id ,node-id
                      :parent-Id ,(or parent-id :null)
                      :content ,content
                      :uid ,uid
                      :client-Id ,client-id)))))))


(defun handle-ws-node-update (ws data)
  (let* ((id (gethash "node-id" data))
         ;; WS でも「送られたかどうか」を見る
         (has-parent-id (nth-value 1 (gethash "parent-id" data)))
         (parent-id (gethash "parent-id" data))
         (content (gethash "content" data))
         (client-id (gethash "client-id" data))
         ;; REST と同じ JSON 用 parent-id
         (json-parent-id
          (if parent-id
              parent-id
              :null)))

    ; (format *error-output*
    ;     "[WS] Update params: id=~A, has-parent-id=~A, parent-id=~A, content=~A, client-id=~A~%"
    ;   id has-parent-id parent-id content client-id)

    (when id
          ;; ① DB更新（REST と同じ）
          (let ((updated-node (models.nodes:update-node
                               id
                               :content content
                               :parent-id parent-id
                               :parent-id-specified-p has-parent-id)))

            ;; 返り値を表示
            (format *error-output* "[WS] Updated node result: ~A~%" updated-node)

            ;; ② map-uuid 特定
            (let ((map-uuid (controllers.nodes:node-id->map-uuid id)))
              (when map-uuid
                    ;; updated-node から値を取得
                    (let ((db-id (getf updated-node :id))
                          (db-content (getf updated-node :content))
                          (db-parent-id (getf updated-node :parent-id)))
                      ;; ③ WSブロードキャスト
                      (ws-utils:ws-broadcast-to-target
                       (format nil "map-~A" map-uuid)
                       (jonathan:to-json
                        `(:type "NODE-UPDATED"
                                :node-Id ,db-id
                                :content ,db-content
                                :parent-Id ,(if db-parent-id db-parent-id :null)
                                :client-Id ,client-id))))))))))

(defun handle-ws-node-delete (ws data)
  (let* ((id (gethash "node-id" data))
         (client-id (gethash "client-id" data))
         (map-uuid (controllers.nodes:node-id->map-uuid id)))

    (format *error-output*
        "[WS] Delete params: node-id=~A, client-id=~A, map-uuid=~A~%"
      id client-id map-uuid)

    (when id
          ;; ① DB削除（論理削除でも物理削除でも可）
          (models.nodes:delete-node id)

          ;; ② map-uuid があれば WS broadcast
          (when map-uuid
                (ws-utils:ws-broadcast-to-target
                 (format nil "map-~A" map-uuid)
                 (jonathan:to-json
                  `(:type "NODE-DELETED"
                          :node-Id ,id
                          :client-Id ,client-id)))))))

(defun ws-on-close (ws)
  (ws-utils:ws-close-handler ws))
