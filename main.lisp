(in-package :cl-user)

(defparameter *dev-mode* t)

(defun dev-reloader (app)
  (lambda (env)
    (when *dev-mode*
          (reload-dev))
    (funcall app env)))

(defvar *file-mod-times* (make-hash-table :test 'equal)
        "ファイルごとの最終更新時刻を保持する。")

(defun reload-dev ()
  (dolist (file '("utils/utils"
                  "models/initsql"
                  "models/users"
                  "models/maps"
                  "models/nodes"
                  "models/map-members"
                  "models/map-invitations"
                  "services/mindmaps"
                  "controllers/users"
                  "controllers/maps"
                  "controllers/nodes"
                  "controllers/map-members"
                  "controllers/map-invitations"
                  "utils/env"
                  "utils/server-utils"
                  "controllers/server"))
    (let* ((pathname (asdf:system-relative-pathname "mindmap"
                                                    (format nil "~A.lisp" file)))
           (new-time (file-write-date pathname))
           (old-time (gethash file *file-mod-times* 0)))
      (when (> new-time old-time)
            (format t "~%Reloading ~A...~%" file)
            (handler-case
                (progn
                 (load pathname)
                 (setf (gethash file *file-mod-times*) new-time))
              (error (e)
                (format t "~%✗ Error while loading ~A: ~A~%" file e)
                ;; エラーの場合は更新時刻を更新しない
                (return)))))))


(defun start-mindmap-server ()
  (utils-env:load-env)
  (init-db-utils:init-db)
  (format t "Starting Mindmap server on port 5000...~%")
  (setf *server*
    (clack:clackup
     (dev-reloader websocket-app::*my-app*)
     :server :woo
     :port 5000)))

(start-mindmap-server)

; (defun stop-mindmap-server ()
;   "Mindmap サーバー停止"
;   (websocket-app:stop-app))

; (asdf:load-system :mindmap)
; (start-mindmap-server)
; (stop-mindmap-server)

;  (uiop:run-program "clear" :output *standard-output*)