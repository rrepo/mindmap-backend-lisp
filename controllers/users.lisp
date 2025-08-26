(defpackage :controllers.users
  (:use :cl :jonathan)
  (:export get-users get-user create-user))

(load "./models/users.lisp")
(load "./utils/utils.lisp")

(in-package :controllers.users)

(defun get-users ()
  (let ((users (models.users:get-users)))
    (jonathan:to-json users)))

(defun get-user (json-string)
  (let* ((params (jonathan:parse json-string :keywordize t))
         (uid (getf params :|uid|)))
    (format *error-output* "UID: ~A~%" uid)
    (models.users:get-user uid)))

(defun create-user (json-string)
  (handler-case
      (let* ((data (jonathan:parse json-string :keywordize t))
             (uid (getf data :|uid|))
             (name (getf data :|name|))
             (img (getf data :|img| nil)))
        (models.users:create-user uid name img))
    (error (e)
      (list :error (princ-to-string e)))))

