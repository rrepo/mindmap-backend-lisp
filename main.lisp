(ql:quickload :postmodern)
(use-package :postmodern)
(ql:quickload :split-sequence)
(use-package :split-sequence)
(ql:quickload :jonathan)
(use-package :jonathan)
(ql:quickload '(:clack :woo :websocket-driver-server))
(use-package :clack)
(use-package :websocket-driver)
(ql:quickload :jsown)
(ql:quickload :local-time)
(ql:quickload :frugal-uuid)
(ql:quickload :ironclad)
(ql:quickload :cl-base64)
(ql:quickload :cl-dotenv)

(load "./models/initsql.lisp")
(load "./models/users.lisp")
(load "./models/maps.lisp")
(load "./models/nodes.lisp")
(load "./models/map-members.lisp")
(load "./models/map-invitations.lisp")
(load "./controllers/server.lisp")
(load "./controllers/users.lisp")
(load "./controllers/maps.lisp")
(load "./controllers/nodes.lisp")
(load "./controllers/map-members.lisp")
(load "./controllers/map-invitations.lisp")
(load "./services/mindmaps.lisp")
(load "./utils/utils.lisp")
(load "./utils/env.lisp")
(init-db-utils:init-db)

(utils-env:load-env)

(defvar *failed-files* nil)

(defun safe-load (file)
  "ファイルを安全にロードし、警告は表示だけ、エラーは失敗扱いにする。
   失敗した場合は *FAILED-FILES* に記録する。"
  (let ((ok t))
    (handler-bind
        ((warning
          (lambda (w)
            (format t "⚠ Warning while loading ~A: ~A~%" file w)
            ;; 警告は握りつぶして処理継続
            (muffle-warning w)))
         (error
             (lambda (e)
               (format t "✗ Error loading ~A: ~A~%" file e)
               ;; 失敗リストに追加
               (push (list file e) *failed-files*)
               (setf ok nil)
               (return-from safe-load nil))))
      (load file))
    (when ok
          (format t "✓ Successfully loaded: ~A~%" file)
          t)))

(defun reload-files ()
  "全てのファイルを安全にリロード。
   成功なら T と NIL を返し、エラーがあれば NIL と *FAILED-FILES* を返す。"
  (format t "~%=== Reloading files ===~%")
  (setf *failed-files* nil) ;; リセット
  (let ((results
         (mapcar #'safe-load
           '("./models/initsql.lisp"
             "./models/users.lisp"
             "./models/maps.lisp"
             "./models/nodes.lisp"
             "./models/map-members.lisp"
             "./models/map-invitations.lisp"
             "./controllers/server.lisp"
             "./controllers/users.lisp"
             "./controllers/maps.lisp"
             "./controllers/nodes.lisp"
             "./controllers/map-members.lisp"
             "./controllers/map-invitations.lisp"
             "./services/mindmaps.lisp"
             "./utils/utils.lisp"
             "./utils/env.lisp"))))
    (if (every #'identity results)
        (progn
         (format t "=== Reload complete (OK) ===~%~%")
         (values t nil))
        (progn
         (format t "=== Reload complete (WITH ERRORS) ===~%~%")
         (values nil (reverse *failed-files*))))))

(defun dev-app-with-reload (env)
  (handler-case
      (multiple-value-bind (ok failed-files) (reload-files)
        (if ok
            ;; リロード成功 → 新しいアプリで動作
            (progn
             (setf *last-good-app* websocket-app::*my-app*)
             (funcall *last-good-app* env))
            ;; リロード失敗 → 古いアプリで動かしつつ、レスポンスにも通知
            (progn
             (format t "✗ Reload failed in files: ~A~%" failed-files)
             (if *last-good-app*
                 ;; 古いアプリを使うが、レスポンスに失敗情報を返す
                 `(500
                   (:content-type "text/plain")
                   (,(format nil "Reload failed in files:~%~{ - ~A : ~A~%~}"
                       (mapcar (lambda (f)
                                 (list (first f) (second f)))
                           failed-files))))
                 ;; 最初から動いてない場合
                 '(500
                   (:content-type "text/plain")
                   ("No working app available (reload failed)"))))))
    (error (condition)
      (format t "✗ Error in dev-app: ~A~%" condition)
      '(500 (:content-type "text/plain")
            ("Internal Server Error - Check console for details")))))

(defun start-dev-app-safe (&key (port 5000))
  "エラーハンドリング付きの開発サーバー起動"
  (format t "Starting development server on port ~A with error handling~%" port)

  ;; サーバー起動
  (handler-case
      (clack:clackup #'dev-app-with-reload :server :woo :port port)
    (error (condition)
      (format t "✗ Failed to start server: ~A~%" condition))))

;; 手動リロード用の便利関数
(defun reload ()
  "手動でファイルをリロード"
  (reload-files))

(start-dev-app-safe)

; (print (websocket-app:start-app :port 5000))


; curl -X POST \ -H "Content-Type: application/json" \ -d '{"uid":"u123", "name":"Taro", "img":"http://example.com"}' \ http://localhost:5000/create-user

; curl -X POST -H "Content-Type: application/json" -d '{"uid":"22fdd" , "name":"Tfff" , "img":"http://example.com" }' http://localhost:5000/update-user