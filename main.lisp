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
(load "./controllers/server.lisp")
(load "./utils/utils.lisp")
(init-db-utils:init-db)

(defun dev-app (env)
  (load "./models/initsql.lisp")
  (load "./models/users.lisp")
  (load "./controllers/server.lisp")
  (load "./utils/utils.lisp")
  (funcall websocket-app::*my-app* env))

; リクエストのたびにリロードする
(defun start-dev-app (&key (port 5000))
  (clack:clackup #'dev-app :server :woo :port port))

(start-dev-app)

; (print (websocket-app:start-app :port 5000))


; curl -X POST \
;      -H "Content-Type: application/json" \
;      -d '{"uid":"u123", "name":"Taro", "img":"http://example.com"}' \
;      http://localhost:5000/create-user