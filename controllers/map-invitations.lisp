(defpackage :controllers.map-invitations
  (:use :cl :postmodern)
  (:export :handle-get-map-invitation
           :handle-get-map-invitation-by-token
           :handle-get-map-invitation-by-map-uuid
           :handle-create-map-invitation
           :handle-delete-map-invitation))

(in-package :controllers.map-invitations)

(defun handle-get-map-invitation (env)
  "ID指定で map_member を取得"
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (id (getf params :ID)))
     (when id
           (models.map-invitations:get-invitation id)))))

(defun handle-get-map-invitation-by-token (env)
  "ID指定で map_member を取得"
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (token (getf params :token)))
     (format *error-output* "Fetching invitation for token=~A~%" token)
     (when token
           (models.map-invitations:get-invitation-by-token token)))))

(defun handle-get-map-invitation-by-map-uuid (env)
  "ID指定で map_member を取得"
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (map-uuid (getf params :ID)))
     (format *error-output* "Fetching invitations for map-id=~A~%" map-uuid)
     (when map-uuid
           (models.map-invitations:get-invitations-by-map-uuid map-uuid)))))

(defun handle-create-map-invitation (env)
  "map_id と user_uid を指定して map_member を追加し、生成されたトークンを返す。"
  (utils:with-invalid
   (let* ((params (utils:extract-json-params env))
          (map-uuid (getf params :|map-uuid|))
          (user-uid (getf params :|uid|))
          (expires-at (getf params :|expires-at|)))
     (format *error-output*
         "Creating invitation request: map-id=~A, user-uid=~A, expires-at=~A~%"
       map-uuid user-uid expires-at)
     (when (and map-uuid user-uid)
           (let ((token (if expires-at
                            (models.map-invitations:create-invitation map-uuid user-uid :expires-at expires-at)
                            (models.map-invitations:create-invitation map-uuid user-uid))))
             (list :token token))))))

(defun handle-delete-map-invitation (env)
  "指定 ID の map_member を削除"
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (id (getf params :ID)))
     (when id
           (models.map-invitations:delete-invitation id)
           :success))))
