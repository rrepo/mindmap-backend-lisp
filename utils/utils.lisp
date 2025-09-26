(defpackage :utils
  (:use :cl :jonathan)
  (:import-from :cl-ppcre :split)
  (:import-from :flexi-streams :octets-to-string)
  (:import-from :frugal-uuid :make-v4 :to-string)
  (:import-from :ironclad :make-random-salt)
  (:import-from :cl-base64 :usb8-array-to-base64-string)
  (:export :parse-query-string :parse-request-body-string :safe-parse-json :parse-query-string-plist :header-value :extract-json-params :with-invalid :get-path-param :secure-random-base64 :uuid-string :generate-secure-invite-token))

(in-package :utils)

(defun header-value (headers name)
  (gethash name headers))

(defun parse-request-body-string (input content-length)
  "リクエストボディを読み取って文字列として返す"
  (when (and input content-length (> content-length 0))
        (let ((buffer (make-array content-length :element-type '(unsigned-byte 8))))
          (read-sequence buffer input)
          (flexi-streams:octets-to-string buffer :external-format :utf-8))))

(defun safe-parse-json (json-string)
  "Parse JSON safely. Returns plist or :invalid."
  (handler-case
      (jonathan:parse json-string :keywords-to-read t)
    (error (e)
      (format *error-output* "JSON parse error: ~A~%" e)
      :invalid)))

(defun parse-query-string-alist (qs)
  "クエリ文字列をALISTに変換"
  (when qs
        (mapcar (lambda (pair)
                  (destructuring-bind (k v)
                      (uiop:split-string pair :separator "=")
                    (cons k v)))
            (uiop:split-string qs :separator "&"))))

(defun parse-query-string-plist (qs)
  "クエリ文字列を PLIST に変換"
  (when qs
        (apply #'append
          (mapcar (lambda (pair)
                    (destructuring-bind (k v)
                        (uiop:split-string pair :separator "=")
                      (list (intern (string-upcase k) :keyword) v)))
              (uiop:split-string qs :separator "&")))))

(defmacro with-invalid (&body body)
  `(handler-case
       (progn ,@body) ; ← そのまま返す。nilはnilのまま
     (error (e)
       (format *error-output* "ERROR: ~A~%" e)
       :invalid)))


(defun extract-json-params (env)
  "env からリクエストボディを取り出して JSON を plist に変換する。
   パースに失敗したら :invalid を返す。"
  (with-invalid
   (let* ((headers (getf env :headers))
          (content-length (parse-integer
                            (or (header-value headers "content-length") "0")
                            :junk-allowed t))
          (input (getf env :raw-body))
          (body-string (parse-request-body-string input content-length))
          (params (safe-parse-json body-string)))
     params)))

(defun uuid-string ()
  "新しい UUID を文字列で返す (ハイフンありの標準形式)."
  (to-string (make-v4)))

(defun generate-secure-invite-token (&optional (bytes 32))
  "推奨: Base64 URL-safeで招待トークンを生成"
  (let ((random-bytes (ironclad:make-random-salt bytes)))
    ;; パディングを削除してさらに短く
    (string-right-trim "="
                       (cl-base64:usb8-array-to-base64-string random-bytes :uri t))))
