(defpackage :controllers.users
  (:use :cl :jonathan)
  (:export get-all-users get-user get-users create-user))

(load "./models/users.lisp")
(load "./utils/utils.lisp")

(in-package :controllers.users)

(defun get-user (env)
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (uid (getf params :UID)))
     (when (and uid (not (string= uid "")))
           (models.users:get-user uid)))))

(defun get-users ()
  (utils:with-invalid
   (format *error-output* "get-users called~%")
   (format *error-output* "uids: ~A~%" '("u123" "u1222"))
   (models.users:get-users '("u123" "u1222"))))

(defun get-all-users ()
  (utils:with-invalid
   (models.users:get-all-users)))

(defun create-user (env)
  "env からリクエストボディを取り出してユーザー作成。常に :success または :invalid を返す"
  (utils:with-invalid
   (let* ((params (utils:extract-json-params env))
          (uid (getf params :|uid|))
          (name (getf params :|name|))
          (img (getf params :|img|)))
     (when (and uid name)
           (models.users:create-user uid name img)
           :success))))
