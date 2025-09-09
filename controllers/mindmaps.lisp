(defpackage :controllers.mindmaps
  (:use :cl :jonathan)
  (:export get-all-maps get-map create-map update-map delete-map get-all-nodes create-node update-node delete-node))

(load "./models/maps.lisp")
(load "./models/nodes.lisp")
(load "./utils/utils.lisp")

(in-package :controllers.mindmaps)

(defun get-map (env)
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (id (getf params :ID)))
     (when (and id (not (string= id "")))
           (let* ((map (models.maps:get-map id))
                  (nodes (models.nodes:get-nodes-by-map-id id)))
             (format *error-output* "Map: ~A~%" map)
             (format *error-output* "nodes: ~A~%" nodes)
             ;; map は plist なので append で nodes を追加
             (append map (list :nodes nodes)))))))

(defun get-all-maps ()
  (utils:with-invalid
   (let* ((maps (models.maps:get-all-maps)))
     maps)))

(defun create-map (env)
  "env からリクエストボディを取り出してユーザー作成。常に :success または :invalid を返す"
  (utils:with-invalid
   (let* ((params (utils:extract-json-params env))
          (title (getf params :|title|))
          (uid (getf params :|uid|))
          (visibility (getf params :|visibility|)))
     (when (and title uid visibility)
           (models.maps:create-map title uid visibility)
           :success))))

(defun update-map (env)
  (utils:with-invalid
   (format *error-output* "Update user called~%")
   (let* ((params (utils:extract-json-params env))
          (id (getf params :|id|))
          (uid (getf params :|uid|))
          (title (getf params :|title|))
          (visibility (getf params :|visibility|)))
     (format *error-output* "Update params: id=~A, uid=~A, title=~A, visibility=~A~%" id uid title visibility)
     (models.maps:update-map id :owner-uid uid :title title :visibility visibility)
     :success)))

(defun delete-map (env)
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (id (getf params :ID)))
     (when (and id (not (string= id "")))
           (models.maps:delete-map id)))))

(defun get-all-nodes ()
  (utils:with-invalid
   (let* ((nodes (models.nodes:get-all-nodes)))
     (format *error-output* "All nodes: ~A~%" nodes)
     nodes)))

(defun create-node (env)
  (utils:with-invalid
   (let* ((params (utils:extract-json-params env))
          (map-id (getf params :|map-id|))
          (parent-id (getf params :|parent-id|))
          (uid (getf params :|uid|))
          (content (getf params :|content|)))
     (when (and map-id content uid)
           (models.nodes:create-node map-id parent-id content uid)
           :success))))

(defun update-node (env)
  (utils:with-invalid
   (let* ((params (utils:extract-json-params env))
          (id (getf params :|id|))
          ;; parent-id がクエリに含まれているかどうかを判定
          (has-parent-id (not (null (member :|parent-id| params))))
          (parent-id (when has-parent-id
                           (getf params :|parent-id|)))
          (content (getf params :|content|)))
     (format *error-output*
         "Update params: id=~A, has-parent-id=~A, parent-id=~A, content=~A~%"
       id has-parent-id parent-id content)
     (when id
           (models.nodes:update-node id
                                     ;; parent-id が明示的に指定されている場合だけ渡す
                                     :parent-id (when has-parent-id parent-id)
                                     :content content)
           :success))))


(defun delete-node (env)
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (id (getf params :ID)))
     (when (and id (not (string= id "")))
           (models.nodes:delete-node id)))))