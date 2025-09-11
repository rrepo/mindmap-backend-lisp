(defpackage :models.map-invitations
  (:use :cl :postmodern :local-time)
  (:export :get-invitation
           :get-invitation-by-token
           :get-invitations-by-map-id
           :create-invitation
           :delete-invitation
           :delete-expired-invitations))

(load "./utils/utils.lisp")

(in-package :models.map-invitations)

(defun get-invitation (id)
  "指定IDの招待を取得する。"
  (query
   "SELECT id, map_id, inviter_uid, token, created_at, expires_at
    FROM map_invitations
    WHERE id = $1"
   id :single :plist))

(defun get-invitation-by-token (token)
  "指定トークンの招待を取得する。"
  (query
   "SELECT id, map_id, inviter_uid, token, created_at, expires_at
    FROM map_invitations
    WHERE token = $1"
   token :single :plist))

;; 3. map_id ごとの招待一覧
(defun get-invitations-by-map-id (map-id)
  "指定されたMAP_IDの招待一覧を取得する。"
  (query
   "SELECT id, map_id, inviter_uid, token, created_at, expires_at
    FROM map_invitations
    WHERE map_id = $1
    ORDER BY created_at ASC"
   map-id :plists))

;; 4. 招待を作成
(defun create-invitation (map-id inviter-uid &key (days-valid 7))
  (let* ((token (utils:secure-random-hex 16))
         (expires-at (local-time:adjust-timestamp
                      (local-time:now)
                      (list :day days-valid))))
    (execute
     "INSERT INTO map_invitations (map_id, inviter_uid, token, expires_at)
      VALUES ($1, $2, $3, $4)"
     map-id inviter-uid token expires-at)
    token)) ;; 作成したトークンを返す

;; 5. 招待を削除
(defun delete-invitation (id)
  "指定IDの招待を削除する。"
  (execute
   "DELETE FROM map_invitations WHERE id = $1"
   id))

;; 6. 有効期限切れの招待を削除
(defun delete-expired-invitations ()
  "期限切れの招待を一括削除する。"
  (execute
   "DELETE FROM map_invitations WHERE expires_at < NOW()"))
