(defpackage :services.mindmaps
  (:use :cl)
  (:export get-map-details get-public-maps-with-nodes))

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

(defun ensure-number (value &optional (default 1))
  (or (ignore-errors
        (etypecase value
          (number value)
          (string (parse-integer value))))
      default))

(defun page->offset-zero-based (page &optional (limit 30))
  "page=1 -> offset 0, page=2 -> offset limit"
  (let ((page-num (ensure-number page 1)))
    (* (max 0 (1- page-num)) limit)))

(defun get-public-maps-with-nodes (&key (page 1) (limit 30))
  "Fetch public maps with up to 10 nodes each."
  (let ((offset (page->offset-zero-based page limit)))
    (mapcar
        (lambda (map)
          (let ((map-id (getf map :id)))
            (list*
                :nodes (models.nodes:get-nodes-by-map map-id)
              map)))
        (models.maps:get-latest-public-maps
         :limit limit
         :offset offset))))


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