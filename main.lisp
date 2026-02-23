(in-package :cl-user)

(defparameter *dev-mode* t)

(defvar *reload-error* nil
        "最後のリロードエラーを保持する。")

(defvar *file-mod-times* (make-hash-table :test 'equal)
        "ファイルごとの最終更新時刻を保持する。")

(defun dev-reloader (app)
  (lambda (env)
    (when *dev-mode*
          (reload-dev))
    ;; リロードエラーがあればエラーレスポンスを返す
    (if *reload-error*
        (list 500
              '(:content-type "text/plain; charset=utf-8")
              (list (format nil "Development Error: Failed to reload file~%~%~A"
                      *reload-error*)))
        (funcall app env))))

(defun reload-dev ()
  (dolist (file '("controllers/controllers-package"
                  "utils/utils"
                  "utils/verify"
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
                  "controllers/ws"
                  "utils/server-utils"
                  "utils/ws-utils"
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
                 (setf (gethash file *file-mod-times*) new-time)
                 ;; 成功したらエラーをクリア
                 (setf *reload-error* nil))
              (error (e)
                (format t "~%✗ Error while loading ~A: ~A~%" file e)
                ;; エラー情報を保存（更新時刻は更新しない）
                (setf *reload-error*
                  (format nil "File: ~A~%Error: ~A" file e))
                (return)))))))

(defun start-mindmap-server ()
  (utils-env:load-env)
  (init-db-utils:init-db)

  (if utils-env:*is-dev*
      ;; ===== 開発環境 =====
      (progn
       (format t "Starting Mindmap server (DEV mode) on port 5000...~%")
       (setf *server*
         (clack:clackup
          (dev-reloader websocket-app::*my-app*)
          :server :woo
          :port 5000)))

      ;; ===== 本番環境 =====
      (progn
       (format t "Starting Mindmap server (PROD mode) on port 5000...~%")
       (setf *server*
         (websocket-app:start-app :port 5000)))))


; (start-mindmap-server)
(defun main ()
  (start-mindmap-server))

; rlwrap sbcl --eval '(asdf:load-system :mindmap)'

; (defun stop-mindmap-server ()
;   "Mindmap サーバー停止"
;   (websocket-app:stop-app))

; (asdf:load-system :mindmap)
; (start-mindmap-server)
; (stop-mindmap-server)

;  (uiop:run-program "clear" :output *standard-output*)

; sudo -u postgres psql