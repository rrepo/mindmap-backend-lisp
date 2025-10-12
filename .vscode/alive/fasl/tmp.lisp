(defpackage :controllers.maps
  (:use :cl :jonathan)
  (:export handle-get-all-maps
           handle-get-map
           handle-get-maps-by-uid
           handle-create-map
           handle-update-map
           handle-delete-map
           handle-get-map-details))

(in-package :controllers.maps)
(defun handle-get-map (env)
  (utils:with-invalid
   (format *error-output* "Get map calleddd!!fdfdfd~%")
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

(defun handle-get-maps-by-uid (env)
  (utils:with-invalid
   (format *error-output* "Get map calleddd uid !!fdfdfd~%")
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (id (getf params :ID)))
     (when (and id (not (string= id "")))
           (format *error-output* "Get maps by uid called with ID=~A~%" id)
           (let* ((map (models.maps:get-maps-by-user-uid id)))
             (format *error-output* "Map: ~A~%" map)
             map)))))

(defun handle-get-all-maps ()
  (utils:with-invalid
   (let* ((maps (models.maps:get-all-maps)))
     maps)))

(defun handle-create-map (env)
  "env からリクエストボディを取り出してユーザー作成。常に :success または :invalid を返す"
  (utils:with-invalid
   (let* ((params (utils:extract-json-params env))
          (title (getf params :|title|))
          (uid (getf params :|uid|))
          (visibility (getf params :|visibility|)))
     (when (and title uid visibility)
           (models.maps:create-map title uid visibility)
           :success))))

(defun handle-update-map (env)
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

(defun handle-delete-map (env)
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (id (getf params :ID)))
     (when (and id (not (string= id "")))
           (models.maps:delete-map id)
           (models.nodes:delete-nodes-by-map-id id)
           (models.map-members:delete-map-members-by-map-id id)
           (models.map-invitations:delete-invitations-by-map-id id)
           :success))))

(defun handle-get-map-details (env)
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (id (getf params :ID)))
     (format *error-output* "Get map details called with ID=~A~%" id)
     (when (and id (not (string= id "")))
           (services.mindmaps:get-map-details id)))))