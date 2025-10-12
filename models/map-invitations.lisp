(defpackage :models.map-invitations
  (:use :cl :postmodern :local-time)
  (:export :get-invitation
           :get-invitation-by-token
           :get-invitations-by-map-id
           :create-invitation
           :delete-invitation
           :delete-expired-invitations
           :delete-invitations-by-map-id))

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
(defun create-invitation (map-id inviter-uid &key (expires-at 7))
  "招待を作成。expires-at は日数指定。デフォルトは7日後。"
  (let* ((token (utils:generate-secure-invite-token))
         ;; 7日後のlocal-time:timestamp
         (expires-ts (local-time:timestamp+ (local-time:now) expires-at :day))
         ;; Postgresに入る文字列に変換 (YYYY-MM-DD HH:MM:SS)
         (expires-str (local-time:format-timestring
                       nil expires-ts
                       :format '(:year "-" (:month 2) "-" (:day 2)
                                       " " (:hour 2) ":" (:min 2) ":" (:sec 2)))))
    (execute
     "INSERT INTO map_invitations (map_id, inviter_uid, token, expires_at)
      VALUES ($1, $2, $3, $4)"
     map-id inviter-uid token expires-str)
    token))


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

(defun delete-invitations-by-map-id (map-id)
  "指定された MAP_ID に紐づくすべての招待を削除する。"
  (execute
   "DELETE FROM map_invitations WHERE map_id = $1"
   map-id))