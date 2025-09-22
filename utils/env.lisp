(defpackage :env-utils
  (:use :cl :cl-dotenv)
  (:export))

(in-package :env-utils)

(.env:load-env (merge-pathnames ".env"))

(defvar *backend-token-secret*
        (uiop:getenv "BACKEND_TOKEN_SECRET"))
