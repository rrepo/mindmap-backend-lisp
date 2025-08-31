(defpackage :utils
  (:use :cl :jonathan)
  (:import-from :cl-ppcre :split)
  (:import-from :flexi-streams :octets-to-string)
  (:export :parse-query-string :parse-request-body-string :safe-parse-json :parse-query-string-plist))

(in-package :utils)

(defun parse-request-body-string (input content-length)
  "リクエストボディを読み取って文字列として返す"
  (when (and input content-length (> content-length 0))
        (let ((buffer (make-array content-length :element-type '(unsigned-byte 8))))
          (read-sequence buffer input)
          (flexi-streams:octets-to-string buffer :external-format :utf-8))))

(defun safe-parse-json (json-string)
  "Parse JSON safely. Returns parsed data or (:error <message>)."
  (handler-case
      (jonathan:parse json-string :keywordize t)
    (error (e)
      ;; 失敗したら (:error "メッセージ") を返す
      (list :error (format nil "~A" e)))))

(defun parse-query-string (qs)
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
