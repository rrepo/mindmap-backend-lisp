(defpackage :controllers.map-members
  (:use :cl :jonathan)
  (:export handle-get-map-member
           handle-get-map-members-by-map-id
           handle-get-map-members-by-user-uid
           handle-get-all-map-members
           handle-create-map-member
           handle-delete-map-member))

(in-package :controllers.map-members)

(defun handle-get-map-member (env)
  "ID指定で map_member を取得"
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (id (getf params :ID)))
     (when id
           (models.map-members:get-map-member id)))))

(defun handle-get-map-members-by-map-id (env)
  "MAP_ID 指定でメンバー一覧を取得"
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (map-id (getf params :ID)))
     (when map-id
           (models.map-members:get-map-members-by-map-id map-id)))))

(defun handle-get-map-members-by-user-uid (env)
  "USER_UID 指定でメンバー一覧を取得"
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (user-uid (getf params :ID)))
     (when user-uid
           (models.map-members:get-map-members-by-user-uid user-uid)))))

(defun handle-get-all-map-members ()
  "全ての map_member を取得"
  (format *error-output* "Fetching all map members...~%")
  (utils:with-invalid
   (models.map-members:get-all-map-members)))

(defun handle-create-map-member (env)
  "map_id と user_uid を指定して map_member を追加"
  (utils:with-invalid
   (format *error-output* "Creating map member...~%")
   (let* ((params (utils:extract-json-params env))
          (token (getf params :|token|))
          (invitation (models.map-invitations:get-invitation-by-token token))
          (user-uid (getf params :|uid|))
          (map-id (getf invitation :map-id))
          (_ (format *error-output* "Invitation: ~A~%" invitation))
          (_ (format *error-output* "Map ID: ~A~%" map-id)))
     (format *error-output* "Params: ~A~%" map-id)
     (when (and user-uid map-id)
           (format *error-output* "Creating map member for user ~A and map ~A~%" user-uid map-id)
           (models.map-members:create-map-member map-id user-uid)
           :success))))

(defun handle-delete-map-member (env)
  "指定 ID の map_member を削除"
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (id (getf params :ID)))
     (when id
           (models.map-members:delete-map-member id)
           :success))))
