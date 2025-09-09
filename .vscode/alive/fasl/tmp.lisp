(defpackage :controllers.mindmaps
  (:use :cl :jonathan)
  (:export get-all-maps get-map create-map update-map delete-map))

(load "./models/maps.lisp")
(load "./utils/utils.lisp")

(in-package :controllers.mindmaps)

(defun get-map (env)
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (id (getf params :ID)))
     (when (and id (not (string= id "")))
           (models.maps:get-map id)))))

(defun get-all-maps ()
  (utils:with-invalid
   (let* ((maps (models.maps:get-all-maps)))
     maps)))

(defun create-map (env)
  "env からリクエストボディを取り出してユーザー作成。常に :success または :invalid を返す"
  (utils:with-invalid
   (let* ((params (utils:extract-json-params env))
          (title (getf params :|title|))
          (uid (getf params :|uid|))
          (visibility (getf params :|visibility|)))
     (when (and title uid visibility)
           (models.maps:create-map title uid visibility)
           :success))))

(defun update-map (env)
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

(defun delete-map (env)
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (id (getf params :ID)))
     (when (and id (not (string= id "")))
           (models.maps:delete-map id)))))