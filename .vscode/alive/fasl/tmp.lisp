(in-package :cl-user)

(defparameter *dev-mode* t)

(defun dev-reloader (app)
  (lambda (env)
    (when *dev-mode*
          (ignore-errors
            (asdf:load-system :mindmap :force t)))
    (funcall app env)))

(defun start-mindmap-server ()
  (utils-env:load-env)
  (init-db-utils:init-db)
  (format t "Starting Mindmap server on port 5000...~%")
  (setf *server*
    (clack:clackup
     (dev-reloader websocket-app::*my-app*)
     :server :woo
     :port 5000)))

; (defun stop-mindmap-server ()
;   "Mindmap サーバー停止"
;   (websocket-app:stop-app))

; (asdf:load-system :mindmap)
; (start-mindmap-server)
; (stop-mindmap-server)

;  (uiop:run-program "clear" :output *standard-output*)