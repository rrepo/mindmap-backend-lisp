(defpackage :controllers.users
  (:use :cl :jonathan)
  (:export handle-get-all-users handle-get-user handle-get-users handle-create-user handle-update-user handle-delete-user))

(in-package :controllers.users)

(defun handle-get-user (env)
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (uid (getf params :UID)))
     (when (and uid (not (string= uid "")))
           (models.users:get-user uid)))))

(defun handle-get-users (env)
  "配列パラメータ形式を処理"
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs)))
     (let ((uid-values '()))
       (loop for (key value) on params by #'cddr do
               (when (or (eq key :UID)
                         (and (stringp (symbol-name key))
                              (search "UID" (symbol-name key))))
                     (push value uid-values)))

       (let ((uids (nreverse (remove-if (lambda (uid) (or (null uid) (string= uid ""))) uid-values))))
         (format *error-output* "Array UIDs: ~A~%" uids)
         (when uids
               (if (= (length uids) 1)
                   (list (models.users:get-user (first uids)))
                   (models.users:get-users uids))))))))

(defun handle-get-all-users ()
  (utils:with-invalid
   (format *error-output* "Get all users called~%")
   (models.users:get-all-users)))

(defun handle-create-user (env)
  "env からリクエストボディを取り出してユーザー作成。常に :success または :invalid を返す"
  (utils:with-invalid
   (let* ((params (utils:extract-json-params env))
          (uid (getf params :|uid|))
          (name (getf params :|name|))
          (img (getf params :|img|)))
     (when (and uid name)
           (models.users:create-user uid name img)
           :success))))

(defun handle-update-user (env)
  (utils:with-invalid
   (format *error-output* "Update user called~%")
   (let* ((params (utils:extract-json-params env))
          (uid (getf params :|uid|))
          (name (getf params :|name|))
          (img (getf params :|img|)))
     (format *error-output* "Update params: uid=~A, name=~A, img=~A~%" uid name img)
     (models.users:update-user uid :name name :img img)
     :success)))

(defun handle-delete-user (env)
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (uid (getf params :UID)))
     (format *error-output* "Delete params: uid=~A~%" uid)
     (when (and uid (not (string= uid "")))
           (models.users:delete-user uid)))))