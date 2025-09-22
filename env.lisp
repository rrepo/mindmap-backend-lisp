(ql:quickload :cl-dotenv)

(.env:load-env (merge-pathnames ".env"))
(defvar *api-key* (uiop:getenv "BACKEND_TOKEN_SECRET"))
(print *api-key*)