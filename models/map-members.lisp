(defpackage :models.map-members
  (:use :cl :postmodern)
  (:export get-map-member
           get-map-members
           create-map-member
           update-map-member
           delete-map-member))

(in-package :models.map-members)

(defun get-map-member (id)
  "指定IDのmap_memberを取得する。"
  (postmodern:query
   "SELECT id, map_id, user_uid, created_at, updated_at
    FROM map_members
    WHERE id = $1"
   id))

(defun get-map-members ()
  "すべてのmap_memberを取得する。"
  (postmodern:query
   "SELECT id, map_id, user_uid, created_at, updated_at FROM map_members"
   :rows))

(defun create-map-member (map-id user-uid)
  "map_membersに新しいレコードを挿入する。"
  (postmodern:execute
   "INSERT INTO map_members (map_id, user_uid) VALUES ($1, $2)"
   map-id user-uid))

(defun update-map-member (id new-map-id new-user-uid)
  "map_membersの指定IDのmap_idとuser_uidを更新する。"
  (postmodern:execute
   "UPDATE map_members
    SET map_id = $1,
        user_uid = $2,
        updated_at = CURRENT_TIMESTAMP
    WHERE id = $3"
   new-map-id new-user-uid id))

(defun delete-map-member (id)
  "map_membersの指定IDのレコードを削除する。"
  (postmodern:execute
   "DELETE FROM map_members WHERE id = $1"
   id))
