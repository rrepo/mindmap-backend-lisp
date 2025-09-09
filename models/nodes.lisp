(defpackage :models.nodes
  (:use :cl :postmodern)
  (:export get-all-nodes get-nodes-by-map-id create-node delete-node))

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
  "Insert a new node into the nodes table.
If parent-id is NIL, insert NULL instead."
  (postmodern:execute
   "INSERT INTO nodes (map_id, parent_id, content, user_uid)
    VALUES ($1, $2, $3, $4)"
   map-id
   (if parent-id parent-id :null)
   content
   user-uid))

(defun delete-node (id)
  "Delete a map by its ID."
  (postmodern:execute
   "DELETE FROM nodes WHERE id = $1"
   id))
