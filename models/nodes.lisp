(defpackage :models.nodes
  (:use :cl :postmodern)
  (:export get-all-nodes
           get-nodes-by-map-id get-map-uuid-by-node-id create-node update-node delete-node delete-nodes-by-map-id delete-node-with-descendants get-nodes-by-map get-nodes-by-map-ids))

(in-package :models.nodes)

(defun get-all-nodes ()
  "Fetch all nodes from the nodes table."
  (postmodern:query
   "SELECT * FROM nodes
    ORDER BY id"
   :rows :plists))

(defun get-map-uuid-by-node-id (node-id)
  "Return map.uuid for given node-id, or NIL if not found."
  (postmodern:query
   "SELECT m.uuid
      FROM nodes n
      JOIN maps m ON n.map_id = m.id
     WHERE n.id = $1"
   node-id
   :rows :plists))

(defun get-nodes-by-map-id (map-id)
  "Fetch all nodes belonging to the given map ID."
  (postmodern:query
   "SELECT * FROM nodes
    WHERE map_id = $1
    ORDER BY id"
   map-id :rows :plists))

(defun create-node (map-id parent-id content user-uid)
  "Insert a new node into the nodes table and return its id."
  (caar
    (postmodern:query
     "INSERT INTO nodes (map_id, parent_id, content, user_uid)
     VALUES ($1, $2, $3, $4)
     RETURNING id"
     map-id
     (if parent-id parent-id :null)
     content
     user-uid)))

(defun update-node (id &key content parent-id parent-id-specified-p)
  (cond
   ;; content + parent-id 両方更新
   ((and content parent-id-specified-p)
     (postmodern:query
      "UPDATE nodes SET content = $1, parent_id = $2 WHERE id = $3 RETURNING *"
      content (if parent-id parent-id :null) id
      :plist))

   ;; content のみ更新
   (content
     (postmodern:query
      "UPDATE nodes SET content = $1 WHERE id = $2 RETURNING *"
      content id
      :plist))

   ;; parent-id のみ更新（NULL 含む）
   (parent-id-specified-p
     (postmodern:query
      "UPDATE nodes SET parent_id = $1 WHERE id = $2 RETURNING *"
      (if parent-id parent-id :null) id
      :plist))

   ;; 何も更新しない
   (t
     nil)))


(defun delete-node (id)
  "Delete a map by its ID."
  (postmodern:execute
   "DELETE FROM nodes WHERE id = $1"
   id))

(defun delete-nodes-by-map-id (map-id)
  "指定された MAP_ID に紐づくすべての nodes レコードを削除する。"
  (postmodern:execute
   "DELETE FROM nodes
    WHERE map_id = $1"
   map-id))

(defun delete-node-with-descendants (node-id map-id)
  (postmodern:execute
   "
WITH RECURSIVE descendants AS (
    SELECT id
    FROM nodes
    WHERE id = $1 AND map_id = $2

    UNION ALL

    SELECT n.id
    FROM nodes n
    INNER JOIN descendants d
      ON n.parent_id = d.id
)
DELETE FROM nodes
WHERE id IN (SELECT id FROM descendants)
"
   node-id map-id))

(defun get-nodes-by-map (map-id)
  "Fetch up to 10 nodes for a map."
  (postmodern:query
   "SELECT id, parent_id, content, user_uid, created_at, updated_at
    FROM nodes
    WHERE map_id = $1
    ORDER BY created_at DESC
    LIMIT 10"
   map-id :rows :plist))

(defun get-nodes-by-map-ids (map-ids)
  (let* ((ids (utils:ensure-integers map-ids))
         (in-clause (utils:sql-in-clause ids)))
    (postmodern:query
     (format nil
         "
SELECT
  id,
  map_id,
  parent_id,
  content,
  user_uid,
  created_at,
  updated_at
FROM nodes
WHERE map_id IN (~A)
ORDER BY map_id, created_at DESC
LIMIT 10
" in-clause)
     :rows :plists)))
