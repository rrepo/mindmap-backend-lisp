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
  "Update only the given fields of a node.
If PARENT-ID is explicitly NIL and PARENT-ID-SPECIFIED-P is T, set parent_id to NULL."
  (let ((sets '())
        (params '()))
    ;; content が指定されていれば更新対象に追加
    (when content
          (push "content = $~a" sets)
          (push content params))
    ;; parent-id が指定されていれば更新対象に追加
    (when parent-id-specified-p
          (push "parent_id = $~a" sets)
          (push parent-id params)) ;; NIL が来ればそのまま NULL にマッピングされる

    ;; 実際のクエリを組み立てて実行
    (when sets
          (postmodern:execute
           (format nil
               "UPDATE nodes SET ~{~a~^, ~} WHERE id = $~a"
             (loop for i from 1
                   for set in (reverse sets)
                   collect (format nil set i))
             (+ (length params) 1))
           (append (reverse params) (list id))))))


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