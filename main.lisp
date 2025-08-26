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

(print (websocket-app:start-app :port 5000))


; (postmodern:with-transaction ()
;                              (progn
;                               (models.users:create-user "12345" "one" "init.png")
;                               (models.users:update-user "12345" "updated_name" "updated_img.png")))

; (print (models.users:get-user "12345"))

; \c mindmap

; curl -X POST \
;      -H "Content-Type: application/json" \
;      -d '{"uid":"u123", "name":"Taro", "img":"http://example.com"}' \
;      http://localhost:5000/create-user