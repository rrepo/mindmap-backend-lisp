(defpackage :controllers.nodes
  (:use :cl :jonathan)
  (:export get-all-nodes create-node update-node delete-node))

(in-package :controllers.nodes)

(defun get-all-nodes ()
  (utils:with-invalid
   (let* ((nodes (models.nodes:get-all-nodes)))
     (format *error-output* "All nodes: ~A~%" nodes)
     nodes)))

(defun create-node (env)
  (utils:with-invalid
   (let* ((params (utils:extract-json-params env))
          (map-id (getf params :|map-id|))
          (parent-id (getf params :|parent-id|))
          (uid (getf params :|uid|))
          (content (getf params :|content|)))
     (when (and map-id content uid)
           (models.nodes:create-node map-id parent-id content uid)
           :success))))

(defun update-node (env)
  (utils:with-invalid
   (let* ((params (utils:extract-json-params env))
          (id (getf params :|id|))
          ;; parent-id がクエリに含まれているかどうかを判定
          (has-parent-id (not (null (member :|parent-id| params))))
          (parent-id (when has-parent-id
                           (getf params :|parent-id|)))
          (content (getf params :|content|)))
     (format *error-output*
         "Update params: id=~A, has-parent-id=~A, parent-id=~A, content=~A~%"
       id has-parent-id parent-id content)
     (when id
           (models.nodes:update-node id
                                     :parent-id (when has-parent-id parent-id)
                                     :content content)
           :success))))


(defun delete-node (env)
  (utils:with-invalid
   (let* ((qs (getf env :query-string))
          (params (utils:parse-query-string-plist qs))
          (id (getf params :ID)))
     (when (and id (not (string= id "")))
           (models.nodes:delete-node id)))))