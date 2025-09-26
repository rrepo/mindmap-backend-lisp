(in-package :cl-user)

(defun start-mindmap-server ()
  (utils-env:load-env)
  (init-db-utils:init-db)
  (websocket-app:start-app :port 5000))

; (print (websocket-app:start-app :port 5000))
