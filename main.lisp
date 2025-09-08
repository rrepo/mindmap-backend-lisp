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

(load "./models/initsql.lisp")
(load "./models/users.lisp")
(load "./models/maps.lisp")
(load "./controllers/server.lisp")
(load "./controllers/users.lisp")
(load "./controllers/mindmaps.lisp")
(load "./utils/utils.lisp")
(init-db-utils:init-db)

(defun safe-load (file)
  "ファイルを安全にロードし、警告は表示だけ、エラーは失敗扱いにする"
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
               ;; エラーが出たら失敗扱いにする
               (setf ok nil)
               (return-from safe-load nil))))
      (load file))
    (when ok
          (format t "✓ Successfully loaded: ~A~%" file)
          t)))


(defun reload-files ()
  "全てのファイルを安全にリロード。成功なら T, エラーがあれば NIL を返す"
  (format t "~%=== Reloading files ===~%")
  (let ((results
         (mapcar #'safe-load
           '("./models/initsql.lisp"
             "./models/users.lisp"
             "./models/maps.lisp"
             "./controllers/server.lisp"
             "./controllers/users.lisp"
             "./controllers/mindmaps.lisp"
             "./utils/utils.lisp"))))
    (if (every #'identity results)
        (progn
         (format t "=== Reload complete (OK) ===~%~%")
         t)
        (progn
         (format t "=== Reload complete (WITH ERRORS) ===~%~%")
         nil))))

(defun dev-app-with-reload (env)
  "リクエストごとにファイルをリロードするWebアプリケーション"
  (handler-case
      (progn
       ;; 開発モードでは毎回リロード
       (reload-files)
       ;; 実際のアプリケーション関数を呼び出し
       (funcall websocket-app::*my-app* env))
    (error (condition)
      (format t "✗ Error in dev-app: ~A~%" condition)
      ;; エラーページを返す
      '(500
        (:content-type "text/plain")
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


; (defun dev-app (env)
;   (load "./models/initsql.lisp")
;   (load "./models/users.lisp")
;   (load "./models/maps.lisp")
;   (load "./controllers/server.lisp")
;   (load "./controllers/users.lisp")
;   (load "./controllers/mindmaps.lisp")
;   (load "./utils/utils.lisp")
;   (funcall websocket-app::*my-app* env))

; ; リクエストのたびにリロードする
; (defun start-dev-app (&key (port 5000))
;   (clack:clackup #'dev-app :server :woo :port port))

; (start-dev-app)

; (print (websocket-app:start-app :port 5000))


; curl -X POST \ -H "Content-Type: application/json" \ -d '{"uid":"u123", "name":"Taro", "img":"http://example.com"}' \ http://localhost:5000/create-user

; curl -X POST -H "Content-Type: application/json" -d '{"uid":"22fdd" , "name":"Tfff" , "img":"http://example.com" }' http://localhost:5000/update-user