(defpackage :controllers.map-invitations
  (:use :cl :postmodern)
  (:export :get-map-invitation
           :get-map-invitation-by-token
           :get-map-invitation-by-map-id
           :create-map-invitation
           :delete-map-invitation))

(load "./models/map-invitations.lisp")
(load "./utils/utils.lisp")

(in-package :controllers.map-invitations)

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
     (format *error-output* "Fetching invitation for token=~A~%" token)
     (when token
           (models.map-invitations:get-invitation-by-token token)))))

(defun get-map-invitation-by-map-id (env)
  "ID指定で map_member を取得"
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (map-id (getf params :ID)))
     (format *error-output* "Fetching invitations for map-id=~A~%" map-id)
     (when map-id
           (models.map-invitations:get-invitations-by-map-id map-id)))))

(defun create-map-invitation (env)
  "map_id と user_uid を指定して map_member を追加"
  (utils:with-invalid
   (let* ((params (utils:extract-json-params env))
          (map-id (getf params :|map-id|))
          (user-uid (getf params :|uid|))
          (expires-at (getf params :|expires-at|)))
     (format *error-output* "Creating invitation request: map-id=~A, user-uid=~A, expires-at=~A~%" map-id user-uid expires-at)
     (when (and map-id user-uid)
           (if expires-at
               (models.map-invitations:create-invitation map-id user-uid :expires-at expires-at)
               (models.map-invitations:create-invitation map-id user-uid))
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
