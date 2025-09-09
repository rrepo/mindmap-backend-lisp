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
  "Insert a new user into the users table. Returns :success or an error keyword."
  (postmodern:execute
   "INSERT INTO users (uid, name, img)
          VALUES ($1, $2, $3)"
   uid name img)
  :success :rows :plist)

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

(defun delete-user (id)
  (postmodern:execute
   "DELETE FROM maps WHERE uid = $1"
   id) :rows :plist)

