(defpackage :models.nodes
  (:use :cl :postmodern)
  (:export get-nodes-by-map-id))

(in-package :models.nodes)

(defun get-nodes-by-map (map-id)
  "Fetch all nodes belonging to a given map ID."
  (postmodern:query
   "SELECT id, map_id, content, position_x, position_y, created_at, updated_at
    FROM nodes
    WHERE map_id = $1"
   map-id :rows :plists))

