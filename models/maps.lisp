(defpackage :models.maps
  (:use :cl :postmodern)
  (:export get-map
           get-maps-by-ids
           get-all-maps
           get-map-by-uuid
           get-all-maps-by-user-uid
           get-maps-by-user-uid
           create-map
           update-map
           delete-map
           count-private-maps-by-user-uid
           search-public-maps-by-title
           get-latest-public-maps))

(in-package :models.maps)

(defun get-map (id)
  "Fetch a map by its ID."
  (postmodern:query
   "SELECT id, uuid, title, owner_uid, visibility, created_at, updated_at
    FROM maps
    WHERE id = $1"
   id :rows :plist))

(defun get-maps-by-ids (map-ids)
  "複数のIDでマップを一括取得"
  (postmodern:query
   (format nil "SELECT id, uuid, title, owner_uid, visibility, created_at, updated_at
                  FROM maps
                  WHERE id = ANY($1)")
   (coerce map-ids 'vector) ; PostgreSQLの配列として渡す
   :plists))

(defun get-map-by-uuid (uuid)
  "Fetch a map by its ID."
  (postmodern:query
   "SELECT id, uuid, title, owner_uid, visibility, created_at, updated_at
    FROM maps
    WHERE uuid = $1"
   uuid :rows :plist))

(defun get-all-maps-by-user-uid (user-uid)
  "ユーザーがownerまたはmemberとして関わっているすべてのmapを取得（最適化版）"
  (postmodern:query
   "SELECT DISTINCT m.id, m.uuid, m.title, m.owner_uid, m.visibility, m.created_at, m.updated_at
    FROM maps m
    LEFT JOIN map_members mm ON m.id = mm.map_id
    WHERE m.owner_uid = $1 OR mm.user_uid = $1
    ORDER BY m.created_at DESC"
   user-uid
   :plists))

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
   :rows :plists))

(defun create-map (title owner-uid &optional (visibility "private"))
  "Insert a new map and return its uuid."
  (let ((uuid (utils:uuid-string)))
    (postmodern:query
     "INSERT INTO maps (uuid, title, owner_uid, visibility)
      VALUES ($1, $2, $3, $4)
      RETURNING uuid"
     uuid title owner-uid visibility
     :single)))

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

(defun count-private-maps-by-user-uid (user-uid)
  "指定したユーザーが所有する private マップの数を返す"
  (postmodern:query
   "SELECT COUNT(*) AS count
    FROM maps
    WHERE owner_uid = $1
      AND visibility = 'private'"
   user-uid
   :single))

(defun search-public-maps-by-title (keyword &key (limit 20) (offset 0))
  "public なマップの中から、タイトルが keyword に部分一致するものを検索する"
  (postmodern:query
   "
   SELECT id, uuid, title, owner_uid, visibility, created_at, updated_at
   FROM maps
   WHERE visibility = 'public'
     AND title ILIKE '%' || $1 || '%'
   ORDER BY updated_at DESC
   LIMIT $2 OFFSET $3
   "
   keyword
   limit
   offset
   :plists))

(defun get-latest-public-maps ()
  "Fetch latest 30 public maps."
  (postmodern:query
   "SELECT id, uuid, title, owner_uid, visibility, created_at, updated_at
    FROM maps
    WHERE visibility = 'public'
    ORDER BY created_at DESC
    LIMIT 30"
   :rows :plist))