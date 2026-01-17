(defpackage :controllers.maps
  (:use :cl :jonathan)
  (:export handle-get-all-maps
           handle-get-map
           handle-get-maps-by-uid
           handle-create-map
           handle-update-map
           handle-delete-map
           handle-get-map-details
           handle-count-private-maps
           handle-get-public-maps-by-search
           handle-get-public-maps))

(in-package :controllers.maps)
(defun handle-get-map (env)
  (utils:with-invalid
   (format *error-output* "Get map calleddd!!fdfdfd~%")
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (id (getf params :ID)))
     (when (and id (not (string= id "")))
           (let* ((map (models.maps:get-map id))
                  (nodes (models.nodes:get-nodes-by-map-id id)))
             ;; map は plist なので append で nodes を追加
             (append map (list :nodes nodes)))))))

(defun handle-get-all-maps ()
  (utils:with-invalid
   (let* ((maps (models.maps:get-all-maps)))
     maps)))

(defun handle-create-map (env)
  "env からリクエストボディを取り出してマップ作成。uuid を返す"
  (utils:with-invalid
   (let* ((params (utils:extract-json-params env))
          (title (getf params :|title|))
          (uid (getf params :|uid|))
          (visibility (getf params :|visibility|)))
     (when (and title uid visibility)
           (let ((uuid (models.maps:create-map title uid visibility)))
             uuid)))))

(defun handle-update-map (env)
  (utils:with-invalid
   (format *error-output* "Update user called~%")
   (let* ((params (utils:extract-json-params env))
          (id (getf params :|id|))
          (uid (getf params :|uid|))
          (title (getf params :|title|))
          (visibility (getf params :|visibility|)))
     (format *error-output* "Update params: id=~A, uid=~A, title=~A, visibility=~A~%" id uid title visibility)
     (models.maps:update-map id :owner-uid uid :title title :visibility visibility)
     :success)))

(defun handle-delete-map (env)
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (id (getf params :ID)))
     (when (and id (not (string= id "")))
           (models.maps:delete-map id)
           :success))))

(defun handle-get-map-details (env)
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (id (getf params :ID)))
     (format *error-output* "Get map details called with ID=~A~%" id)
     (when (and id (not (string= id "")))
           (services.mindmaps:get-map-details id)))))


(defun handle-count-private-maps (env)
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (id (getf params :ID)))
     (when (and id (not (string= id "")))
           (models.maps:count-private-maps-by-user-uid id)))))


(defun handle-get-public-maps-by-search (env)
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (search (getf params :search)))
     (when (and search (not (string= search "")))
           (models.maps:search-public-maps-by-title search)))))

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

; (defun handle-get-public-maps (env)
;   (utils:with-invalid
;    (let* ((qs (getf env :query-string))
;           (params (utils:parse-query-string-plist qs))
;           (page (getf params :page))
;           (limit-param (getf params :limit))
;           (limit (ensure-number limit-param 30))
;           (offset (page->offset-zero-based page limit)))
;      (format *error-output* "Getting public maps for page=~A, offset=~A~%" page offset)
;      (models.maps:get-public-maps
;       :limit limit
;       :offset offset))))

(defun handle-get-public-maps (env)
  (utils:with-invalid
   "good"))

(defun handle-get-maps-by-uid (env)
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (id (getf params :ID)))
     (when (and id (not (string= id "")))
           (format *error-output* "Getting maps for user UID=~A~%" id)
           (let ((maps (services.mindmaps:get-related-maps-with-nodes id)))
             maps)))))