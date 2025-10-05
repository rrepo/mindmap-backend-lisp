(defpackage :models.users
  (:use :cl :postmodern)
  (:export get-user get-all-users get-users create-user update-user delete-user))

(in-package :models.users)

(defun get-user (uid)
  (postmodern:query
   "SELECT uid, name, img FROM users WHERE uid = $1"
   uid :rows :plist))

(defun get-users (uids)
  (postmodern:query
   (:select 'uid 'name 'img
          :from 'users
            :where (:in 'uid (:set uids)))
   :plists))

(defun get-all-users ()
  (postmodern:query
   "SELECT id, uid, name, img, created_at, updated_at FROM users"
   :rows :plists))

(defun create-user (uid name &optional (img nil))
  "Insert a new user into the users table.
   If the UID already exists, do nothing and return :success."
  (handler-case
      (progn
       (postmodern:execute
        "INSERT INTO users (uid, name, img)
          VALUES ($1, $2, $3)"
        uid name img)
       :success)
    (postmodern:database-error (e)
                               ;; 重複キーエラーなら握りつぶして :success を返す
                               (let ((msg (postmodern:database-error-message e)))
                                 (if (search "duplicate key value violates unique constraint" msg)
                                     (progn
                                      (format *error-output* "Duplicate UID detected (~A), ignoring.~%" uid)
                                      :success)
                                     ;; 他のDBエラーはそのまま報告
                                     (progn
                                      (format *error-output* "Database error: ~A~%" msg)
                                      :db-error))))))


(defun update-user (uid &key name img)
  (when (or name img)
        (cond
         ((and name img)
           (postmodern:execute "UPDATE users SET name = $2, img = $3, updated_at = NOW() WHERE uid = $1"
                               uid name img))
         (name
           (postmodern:execute "UPDATE users SET name = $2, updated_at = NOW() WHERE uid = $1"
                               uid name))
         (img
           (postmodern:execute "UPDATE users SET img = $2, updated_at = NOW() WHERE uid = $1"
                               uid img)))))

(defun delete-user (uid)
  (postmodern:execute
   "DELETE FROM users WHERE uid = $1"
   uid) :rows :plist)
