(defpackage :utils-env
  (:use :cl)
  (:export :*backend-token-secret* :*ws-token-secret* :*frontend-url* :*is-dev*  :load-env))

(in-package :utils-env)

(defvar *backend-token-secret* nil)
(defvar *ws-token-secret* nil)
(defvar *frontend-url* nil)
(defvar *is-dev* nil)

;; cl-dotenv を使って環境変数をロードする関数
;; cl-dotenv を使って環境変数をロードする関数
;; cl-dotenv を使って環境変数をロードする関数
(defun load-env ()
  (format *error-output* "Loading .env file~%")
  (let ((env-path (merge-pathnames ".env"
                                   (asdf:system-source-directory
                                     (asdf:find-system :mindmap)))))
    (format *error-output* "Reading .env from: ~A~%" env-path)

    (let ((env-vars (cl-dotenv:read-env env-path)))
      (maphash (lambda (key value)
                 (when (and key value
                            (not (string= key ""))
                            (not (string= value "")))
                       ;; CRやLFを除去
                       (let ((clean-key (string-trim '(#\Return #\Newline #\Space) key))
                             (clean-value (string-trim '(#\Return #\Newline #\Space) value)))
                         (setf (uiop:getenv clean-key) clean-value)
                         (format *error-output* "Set ~A=[~A]~%" clean-key clean-value))))
               env-vars)))

  (setf *backend-token-secret* (uiop:getenv "BACKEND_TOKEN_SECRET"))
  (setf *ws-token-secret* (uiop:getenv "WS_TOKEN_SECRET"))
  (setf *frontend-url* (uiop:getenv "FRONTEND_URL"))
  (setf *is-dev*
    (let ((val (uiop:getenv "IS_DEV")))
      (when val
            (let ((up (string-upcase val)))
              (or (string= up "1")
                  (string= up "T")
                  (string= up "TRUE"))))))
  (format *error-output* "backend token!!!!! [~A]~%" *backend-token-secret*))