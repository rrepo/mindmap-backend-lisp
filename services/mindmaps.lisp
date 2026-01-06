(defpackage :services.mindmaps
  (:use :cl)
  (:export get-map-details get-latest-public-maps-with-nodes))

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
                  :users users)))))

(defun get-latest-public-maps-with-nodes ()
  "Fetch latest 30 public maps with up to 10 nodes each."
  (mapcar
      (lambda (map)
        (let ((map-id (getf map :id)))
          (append
            map
            (list :nodes (get-nodes-by-map map-id)))))
      (get-latest-public-maps)))

; (defun get-all-maps-by-user-uid (user-uid)
;   "ユーザーがownerまたはmemberとして関わっているすべてのmapを取得"
;   (let* ((owner-maps (models.maps:get-maps-by-user-uid user-uid))
;          (members (models.map-members:get-map-members-by-user-uid user-uid))
;          (member-map-ids (mapcar (lambda (m) (getf m :|MAP-ID|)) members))
;          ;; 一括取得に変更
;          (member-maps (if member-map-ids
;                           (models.maps:get-maps-by-ids member-map-ids)
;                           nil))
;          (all-maps (remove-duplicates
;                        (append owner-maps member-maps)
;                      :test #'equal
;                      :key (lambda (m) (getf m :|ID|)))))
;     all-maps))