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

   (t
     (format *error-output*
         "[WS] Unknown type: ~A~%" type))))

(defun handle-ws-subscribe (ws data)
  (let ((target (gethash "target" data)))
    (when target
          (ws-utils:ws-subscribe ws target))))

(defun handle-ws-unsubscribe (ws data)
  (let ((target (gethash "target" data)))
    (when target
          (ws-utils:ws-unsubscribe ws target))))

(defun handle-ws-node-update (ws data)
  (let* ((id (gethash "node-id" data))
         ;; WS でも「送られたかどうか」を見る
         (has-parent-id (nth-value 1 (gethash "parentId" data)))
         (parent-id (gethash "parentId" data))
         (content (gethash "content" data))
         ;; REST と同じ JSON 用 parent-id
         (json-parent-id
          (if parent-id
              parent-id
              :null)))

    (format *error-output*
        "[WS] Update params: id=~A, has-parent-id=~A, parent-id=~A, content=~A~%"
      id has-parent-id parent-id content)

    (when id
          ;; ① DB更新（REST と同じ）
          (models.nodes:update-node
           id
           :content content
           :parent-id parent-id
           :parent-id-specified-p has-parent-id)

          ;; ② map-uuid 特定
          (let ((map-uuid (controllers.nodes:node-id->map-uuid id)))
            (when map-uuid
                  ;; ③ WSブロードキャスト
                  (ws-utils:ws-broadcast-to-target
                   (format nil "map-~A" map-uuid)
                   (jonathan:to-json
                    `(:type "NODE-UPDATED"
                            :node-Id ,id
                            :content ,content
                            :parent-Id ,json-parent-id))))))))


(defun ws-on-close (ws)
  (ws-utils:ws-close-handler ws))
