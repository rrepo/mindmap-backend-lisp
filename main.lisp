; (in-package :cl-user)

; (defun start-mindmap-server ()
;   (utils-env:load-env)
;   (init-db-utils:init-db)
;   (format t "Starting server on port 5000...!!!!!!!!!!~%")
;   (websocket-app:start-app :port 5000))


; (start-mindmap-server)

; ; (print (websocket-app:start-app :port 5000))

(in-package :cl-user)

(defun start-mindmap-server ()
  "Mindmap サーバー起動（DB初期化も含む）"
  (websocket-app:stop-app)
  (utils-env:load-env)
  (init-db-utils:init-db)
  (format t "Starting Mindmap server on port 5000...~%")
  (websocket-app:start-app :port 5000))

(defun stop-mindmap-server ()
  "Mindmap サーバー停止"
  (websocket-app:stop-app))

; (asdf:load-system :mindmap)
; (start-mindmap-server)
; (stop-mindmap-server)