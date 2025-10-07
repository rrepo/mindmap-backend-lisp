(defpackage :services.mindmaps
  (:use :cl)
  (:export get-map-details))

(in-package :services.mindmaps)

(defun get-map-details (map-uuid)
  "指定 map-id の map と関連する情報を plist で返す"
  (when map-uuid
        (let* ((map (models.maps:get-map-by-uuid map-uuid))
               (map-id (getf map :id))
               (nodes (models.nodes:get-nodes-by-map-id map-id))
               (members (models.map-members:get-map-members-by-map-id map-id))
               (owner-uid (getf map :owner-uid))
               (node-uids (mapcar (lambda (n) (getf n :user-uid)) nodes))
               (member-uids (mapcar (lambda (m) (getf m :user-uid)) members))
               (all-uids (remove-duplicates
                             (append (list owner-uid) node-uids member-uids)
                           :test #'string=))
               (users (models.users:get-users all-uids)))
          ;; --- まとめて返す ---
          (append map
            (list :nodes nodes
                  :members members
                  :users users)))))