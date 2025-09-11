(defpackage :models.map-invitations
  (:use :cl :postmodern)
  (:export))

(load "./models/map-invitations.lisp")
(load "./utils/utils.lisp")

(in-package :models.map-invitations)

(defun get-map-invitation (env)
  "ID指定で map_member を取得"
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (id (getf params :ID)))
     (when id
           (models.map-invitations:get-invitation id)))))

(defun get-map-invitation-by-token (env)
  "ID指定で map_member を取得"
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (token (getf params :token)))
     (when token
           (models.map-invitations:get-invitation-by-token token)))))

(defun get-map-invitation-by-map-id (env)
  "ID指定で map_member を取得"
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (map-id (getf params :ID)))
     (when map-id
           (models.map-invitations:get-invitation-by-token map-id)))))

(defun create-map-invitation (env)
  "map_id と user_uid を指定して map_member を追加"
  (utils:with-invalid
   (let* ((params (utils:extract-json-params env))
          (map-id (getf params :|map-id|))
          (user-uid (getf params :|uid|))
          (expires-at (getf params :|expires-at|)))
     (when (and map-id user-uid)
           (models.map-invitations:create-invitation map-id user-uid expires-at)
           :success))))

(defun delete-map-invitation (env)
  "指定 ID の map_member を削除"
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (id (getf params :ID)))
     (when id
           (models.map-invitations:delete-invitation id)
           :success))))
