(defpackage :models.map-members
  (:use :cl :postmodern)
  (:export get-map-member
           get-all-map-members
           get-map-members-by-map-id
           get-map-members-by-user-uid
           create-map-member
           delete-map-member
           delete-map-members-by-map-id))

(in-package :models.map-members)

(defun get-map-member (id)
  "指定IDのmap_memberを取得する。"
  (postmodern:query
   "SELECT id, map_id, user_uid, created_at, updated_at
    FROM map_members
    WHERE id = $1"
   id :rows :plist))

(defun get-map-members-by-map-id (map-id)
  "指定されたMAP_IDに属するメンバー一覧を取得する。"
  (postmodern:query
   "SELECT id, map_id, user_uid, created_at, updated_at
    FROM map_members
    WHERE map_id = $1
    ORDER BY created_at ASC"
   map-id
   :rows :plists))

(defun get-map-members-by-user-uid (user-uid)
  "指定されたUSER_UIDに属するmap_membersの一覧を取得する。"
  (postmodern:query
   "SELECT id, map_id, user_uid, created_at, updated_at
    FROM map_members
    WHERE user_uid = $1
    ORDER BY created_at ASC"
   user-uid
   :rows :plists))

(defun get-all-map-members ()
  "すべてのmap_memberを取得する。"
  (postmodern:query
   "SELECT id, map_id, user_uid, created_at, updated_at FROM map_members"
   :rows :plists))

(defun create-map-member (map-id user-uid)
  "map_membersに新しいレコードを挿入する。すでに存在する場合は何もしない。"
  (postmodern:execute
   "INSERT INTO map_members (map_id, user_uid)
    VALUES ($1, $2)
    ON CONFLICT (map_id, user_uid) DO NOTHING"
   map-id user-uid))

; (defun update-map-member (id new-map-id new-user-uid)
;   "map_membersの指定IDのmap_idとuser_uidを更新する。"
;   (postmodern:execute
;    "UPDATE map_members
;     SET map_id = $1,
;         user_uid = $2,
;         updated_at = CURRENT_TIMESTAMP
;     WHERE id = $3"
;    new-map-id new-user-uid id))

(defun delete-map-member (map-id user-uid)
  "指定された map_id と user_uid に一致する map_members のレコードを削除する。"
  (postmodern:execute
   "DELETE FROM map_members
    WHERE map_id = $1 AND user_uid = $2"
   map-id user-uid))

(defun delete-map-members-by-map-id (map-id)
  "指定された MAP_ID に紐づくすべての map_members レコードを削除する。"
  (postmodern:execute
   "DELETE FROM map_members
    WHERE map_id = $1"
   map-id))