(defpackage :models.users
  (:use :cl :postmodern)
  (:export get-user get-users create-user update-user delete-user))

(in-package :models.users)

(defun get-user (uid)
  (postmodern:query
   "SELECT uid, name, img FROM users WHERE uid = $1"
   uid :rows :plist))

(defun get-users ()
  (postmodern:query
   "SELECT id, uid, name, img, created_at, updated_at FROM users"
   :rows :plist))

(defun create-user (uid name &optional (img nil))
  "Insert a new user into the users table. Returns :success or an error keyword."
  (postmodern:execute
   "INSERT INTO users (uid, name, img)
          VALUES ($1, $2, $3)"
   uid name img)
  :success :rows :plist)

(defun update-user (uid new-name new-img)
  "Update the name and img of a user identified by UID."
  (postmodern:execute
   "UPDATE users
    SET name = $1,
        img = $2
    WHERE uid = $3"
   new-name new-img uid) :rows :plist)

(defun delete-user (uid)
  (postmodern:execute
   "DELETE FROM users WHERE uid = $1"
   uid) :rows :plist)
