(defpackage :controllers.nodes
  (:use :cl :jonathan)
  (:export handle-get-all-nodes handle-create-node handle-update-node handle-delete-node handle-delete-node-descendants))

(in-package :controllers.nodes)

(defun handle-get-all-nodes ()
  (utils:with-invalid
   (let* ((nodes (models.nodes:get-all-nodes)))
     (format *error-output* "All nodes: ~A~%" nodes)
     nodes)))

(defun handle-create-node (env)
  (utils:with-invalid
   (let* ((params (utils:extract-json-params env))
          (map-id (getf params :|map-id|))
          (parent-id (getf params :|parent-id|))
          (uid (getf params :|uid|))
          (content (getf params :|content|)))
     (format *error-output*
         "Create params: map-id=~A, parent-id=~A, uid=~A, content=~A~%"
       map-id parent-id uid content)
     (when (and map-id content uid)
           (models.nodes:create-node map-id parent-id content uid)))))

(defun node-id->map-uuid (node-id)
  (let ((rows (models.nodes:get-map-uuid-by-node-id node-id)))
    (when rows
          (getf (first rows) :uuid))))

(defun handle-update-node (env)
  (utils:with-invalid
   (let* ((params (utils:extract-json-params env))
          (id (getf params :|id|))
          (has-parent-id (not (null (member :|parent-id| params))))
          (parent-id (when has-parent-id
                           (getf params :|parent-id|)))
          (content (getf params :|content|)))

     (format *error-output*
         "Update params: id=~A, has-parent-id=~A, parent-id=~A, content=~A~%"
       id has-parent-id parent-id content)

     (when id
           ;; ① DB更新
           (models.nodes:update-node
            id
            :content content
            :parent-id parent-id
            :parent-id-specified-p has-parent-id)

           ;; ② map-uuid 特定
           (let ((map-uuid (node-id->map-uuid id)))
             (format *error-output* "!!!mapuuid=~A~%" map-uuid)
             (when map-uuid
                   ;; ③ WSブロードキャスト (O(サブスクライバー数))
                   ;; ここで target を "map" + map-uuid に統一
                   (websocket-app:ws-broadcast-to-target
                    (format nil "map-~A" map-uuid)
                    (jonathan:to-json
                     `(:type "NODE_UPDATED"
                             :nodeId ,id
                             :content ,content
                             :parentId ,parent-id))))))

     :success)))

(defun handle-delete-node (env)
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (id (getf params :ID)))
     (when (and id (not (string= id "")))
           (models.nodes:delete-node id)))))

(defun handle-delete-node-descendants (env)
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (id (getf params :ID))
          (map-id (getf params :MAP-ID)))
     (format *error-output*
         "Delete node descendants params: id=~A, map-id=~A~%"
       id map-id)
     (when (and id (not (string= id "")))
           (models.nodes:delete-node-with-descendants id map-id)))))