(defpackage :utils-env
  (:use :cl)
  (:export :*backend-token-secret* :load-env)) ;; ← export するシンボルを明示

(in-package :utils-env)

(defvar *backend-token-secret* nil)

;; 環境変数をロードする関数にしたほうが安全
(defun load-env ()
  (format *error-output* "Loading .env file~%")
  (.env:load-env (merge-pathnames ".env"))
  (setf *backend-token-secret* (uiop:getenv "BACKEND_TOKEN_SECRET")))
