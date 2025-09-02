(defpackage :controllers.users
  (:use :cl :jonathan)
  (:export get-users get-user create-user))

(load "./models/users.lisp")
(load "./utils/utils.lisp")

(in-package :controllers.users)

(defmacro with-invalid (&body body)
  `(handler-case
       (or (progn ,@body)
           :invalid)
     (error (e)
       (format *error-output* "ERROR: ~A~%" e)
       :invalid)))

(defun get-users ()
  (with-invalid
   (models.users:get-users)))

(defun get-user (env)
  (with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (uid (getf params :UID)))
     (when (and uid (not (string= uid "")))
           (models.users:get-user uid)))))

(defun create-user (env)
  "env からリクエストボディを取り出してユーザー作成。常に :success または :invalid を返す"
  (with-invalid
   (let* ((headers (getf env :headers))
          (content-length (parse-integer
                            (or (utils:header-value headers "content-length") "0")
                            :junk-allowed t))
          (input (getf env :raw-body))
          (body-string (utils:parse-request-body-string input content-length))
          (params (utils:safe-parse-json body-string))
          (uid (getf params :|uid|))
          (name (getf params :|name|))
          (img (getf params :|img|)))
     (when (and uid name)
           (models.users:create-user uid name img)
           :success))))
