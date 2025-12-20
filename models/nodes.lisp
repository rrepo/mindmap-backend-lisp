(defpackage :models.nodes
  (:use :cl :postmodern)
  (:export get-all-nodes get-nodes-by-map-id create-node update-node delete-node delete-nodes-by-map-id))

(in-package :models.nodes)

(defun get-all-nodes ()
  "Fetch all nodes from the nodes table."
  (postmodern:query
   "SELECT * FROM nodes
    ORDER BY id"
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
     (postmodern:execute
      "UPDATE nodes SET content = $1, parent_id = $2 WHERE id = $3"
      content parent-id id))

   ;; content のみ更新
   (content
     (postmodern:execute
      "UPDATE nodes SET content = $1 WHERE id = $2"
      content id))

   ;; parent-id のみ更新（NULL 含む）
   (parent-id-specified-p
     (postmodern:execute
      "UPDATE nodes SET parent_id = $1 WHERE id = $2"
      parent-id id))

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