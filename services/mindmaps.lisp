(defpackage :services.mindmaps
  (:use :cl)
  (:export get-map-detiels))

(load "./utils/utils.lisp")
(load "./models/nodes.lisp")
(load "./models/maps.lisp")
(load "./models/map-members.lisp")
(load "./models/map-invitations.lisp")

(in-package :services.mindmaps)

(defun get-map-detiels (map-id)
  "指定 map-id の map と関連する情報を plist で返す"
  (when map-id
        (let* ((map (models.maps:get-map map-id))
               (nodes (models.nodes:get-nodes-by-map-id map-id))
               (members (models.map-members:get-map-members-by-map-id map-id))
               ;; --- users を集める ---
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