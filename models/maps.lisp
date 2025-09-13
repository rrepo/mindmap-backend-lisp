(defpackage :models.maps
  (:use :cl :postmodern)
  (:export get-map
           get-all-maps
           get-maps-by-user-uid
           create-map
           update-map
           delete-map))

(in-package :models.maps)
(load "./utils/utils.lisp")

(defun get-map (id)
  "Fetch a map by its ID."
  (postmodern:query
   "SELECT id, uuid, title, owner_uid, visibility, created_at, updated_at
    FROM maps
    WHERE id = $1"
   id :rows :plist))

(defun get-all-maps ()
  "Fetch all maps."
  (postmodern:query
   "SELECT id, uuid, title, owner_uid, visibility, created_at, updated_at
    FROM maps"
   :rows :plists))

(defun get-maps-by-user-uid (owner-uid)
  "Fetch all maps belonging to a user specified by owner UID."
  (postmodern:query
   "SELECT id, uuid, title, owner_uid, visibility, created_at, updated_at
    FROM maps
    WHERE owner_uid = $1"
   owner-uid
   :rows :plist))

(defun create-map (title owner-uid &optional (visibility "private"))
  "Insert a new map."
  (postmodern:execute
   "INSERT INTO maps (uuid, title, owner_uid, visibility)
    VALUES ($1, $2, $3, $4)"
   (utils:uuid-string) title owner-uid visibility))

(defun update-map (id &key title owner-uid visibility)
  "Update only the given fields of a map."
  (when (or title owner-uid visibility)
        (cond
         ;; すべて指定された場合
         ((and title owner-uid visibility)
           (postmodern:execute
            "UPDATE maps SET title = $2, owner_uid = $3, visibility = $4, updated_at = NOW() WHERE id = $1"
            id title owner-uid visibility))

         ;; title + owner-uid
         ((and title owner-uid)
           (postmodern:execute
            "UPDATE maps SET title = $2, owner_uid = $3, updated_at = NOW() WHERE id = $1"
            id title owner-uid))

         ;; title + visibility
         ((and title visibility)
           (postmodern:execute
            "UPDATE maps SET title = $2, visibility = $3, updated_at = NOW() WHERE id = $1"
            id title visibility))

         ;; owner-uid + visibility
         ((and owner-uid visibility)
           (postmodern:execute
            "UPDATE maps SET owner_uid = $2, visibility = $3, updated_at = NOW() WHERE id = $1"
            id owner-uid visibility))

         ;; title だけ
         (title
           (postmodern:execute
            "UPDATE maps SET title = $2, updated_at = NOW() WHERE id = $1"
            id title))

         ;; owner-uid だけ
         (owner-uid
           (postmodern:execute
            "UPDATE maps SET owner_uid = $2, updated_at = NOW() WHERE id = $1"
            id owner-uid))

         ;; visibility だけ
         (visibility
           (postmodern:execute
            "UPDATE maps SET visibility = $2, updated_at = NOW() WHERE id = $1"
            id visibility)))))


(defun delete-map (id)
  "Delete a map by its ID."
  (postmodern:execute
   "DELETE FROM maps WHERE id = $1"
   id))
