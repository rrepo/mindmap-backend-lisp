(defpackage :services.mindmaps
  (:use :cl)
  (:export get-map-details get-public-maps get-related-maps-with-nodes))

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

(defun extract-map-ids (maps)
  (mapcar (lambda (m) (getf m :id)) maps))

(defun attach-nodes (maps nodes)
  (let ((table (make-hash-table :test #'eql)))
    (dolist (n nodes)
      (let ((mid (getf n :map-id))) ;; ← これで取れる
        (push n (gethash mid table))))

    (mapcar
        (lambda (m)
          (let* ((mid (getf m :id))
                 (ns (gethash mid table)))
            (list* :nodes (subseq (or ns '()) 0 (min 10 (length ns)))
              m)))
        maps)))

(defun get-related-maps-with-nodes (user-uid)
  "ユーザーがownerまたはmemberとして関わっているすべてのmapを取得し、nodesを付与して返す"
  (when user-uid
        (let* ((maps (models.maps:get-related-maps user-uid))
               (map-ids (extract-map-ids maps))
               (nodes (models.nodes:get-nodes-by-map-ids map-ids)))
          (attach-nodes maps nodes))))

(defun get-public-maps (&key (limit 30) (offset 0))
  "公開マップを取得し、nodesを付与して返す"
  (let* ((maps (models.maps:get-latest-public-maps
                :limit limit
                :offset offset))
         (map-ids (extract-map-ids maps))
         (nodes (models.nodes:get-nodes-by-map-ids map-ids)))
    (attach-nodes maps nodes)))
