(defpackage :models.maps
  (:use :cl :postmodern)
  (:export get-map
           get-maps
           get-maps-by-user-uid
           create-map
           update-map
           delete-map))

(in-package :models.maps)

(defun get-map (id)
  "Fetch a map by its ID."
  (postmodern:query
   "SELECT id, title, owner_uid, visibility, created_at, updated_at
    FROM maps
    WHERE id = $1"
   id))

(defun get-maps ()
  "Fetch all maps."
  (postmodern:query
   "SELECT id, title, owner_uid, visibility, created_at, updated_at
    FROM maps"
   :rows))

(defun get-maps-by-user-uid (owner-uid)
  "Fetch all maps belonging to a user specified by owner UID."
  (postmodern:query
   "SELECT id, title, owner_uid, visibility, created_at, updated_at
    FROM maps
    WHERE owner_uid = $1"
   owner-uid
   :rows))

(defun create-map (title owner-uid &optional (visibility "public"))
  "Insert a new map."
  (postmodern:execute
   "INSERT INTO maps (title, owner_uid, visibility)
    VALUES ($1, $2, $3)"
   title owner-uid visibility))

(defun update-map (id new-title new-owner-uid new-visibility)
  "Update title, owner_uid, and visibility of a map."
  (postmodern:execute
   "UPDATE maps
    SET title = $1,
        owner_uid = $2,
        visibility = $3,
        updated_at = CURRENT_TIMESTAMP
    WHERE id = $4"
   new-title new-owner-uid new-visibility id))

(defun delete-map (id)
  "Delete a map by its ID."
  (postmodern:execute
   "DELETE FROM maps WHERE id = $1"
   id))
