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

(defun get-user (params)
  (with-invalid
   (let ((uid (getf params :UID)))
     (when (and uid (not (string= uid "")))
           (models.users:get-user uid)))))

(defun create-user (json-string)
  (handler-case
      (let* ((data (jonathan:parse json-string :keywordize t))
             (uid (getf data :|uid|))
             (name (getf data :|name|))
             (img (getf data :|img| nil)))
        (models.users:create-user uid name img))
    (error (e)
      (list :error (princ-to-string e)))))
