(defpackage :verify-token
  (:use :cl :jonathan :cl-base64 :babel)
  (:export :authenticate-and-get-uid))

(in-package :verify-token)

;; 必要なライブラリの読み込み
(ql:quickload '(:jonathan :cl-base64 :babel))

(defvar *token* "eyJhbGciOiJSUzI1NiIsImtpZCI6IjA1NTc3MjZmYWIxMjMxZmEyZGNjNTcyMWExMDgzZGE2ODBjNGE3M2YiLCJ0eXAiOiJKV1QifQ.eyJuYW1lIjoi5Yeq5pyI6auY5qmLIiwicGljdHVyZSI6Imh0dHBzOi8vbGgzLmdvb2dsZXVzZXJjb250ZW50LmNvbS9hL0FDZzhvY0swM0E3c1JIWVh0NG1uSGxvc0ctdUk2Q09DRzVFTGpXVTNwNkFXT256TlZ6QzhXcGM9czk2LWMiLCJpc3MiOiJodHRwczovL3NlY3VyZXRva2VuLmdvb2dsZS5jb20vbGlzcC1lMWU5MiIsImF1ZCI6Imxpc3AtZTFlOTIiLCJhdXRoX3RpbWUiOjE3NTc5NDc4MDQsInVzZXJfaWQiOiJPdFVVMHZDMVFKU1VYVVNESmpWVjNadTR2MUUzIiwic3ViIjoiT3RVVTB2QzFRSlNVWFVTREpqVlYzWnU0djFFMyIsImlhdCI6MTc1ODk5MTMzNywiZXhwIjoxNzU4OTk0OTM3LCJlbWFpbCI6InJlcG9yZXBvMjU0QGdtYWlsLmNvbSIsImVtYWlsX3ZlcmlmaWVkIjp0cnVlLCJmaXJlYmFzZSI6eyJpZGVudGl0aWVzIjp7Imdvb2dsZS5jb20iOlsiMTAzMTIwMjQzMDkyNzc3MDQwMjcxIl0sImVtYWlsIjpbInJlcG9yZXBvMjU0QGdtYWlsLmNvbSJdfSwic2lnbl9pbl9wcm92aWRlciI6Imdvb2dsZS5jb20ifX0.DecQ6-J3gLjobEHZJOT-WYBUL20Nh7ofYra3UBdcTk07q2HOgRvdN7ZsJu8y8hE5HM55sS0OkbWy943wL_qag3EaFjvuFsSrHNJUD_JDx3P45phhdHv6Gcp-bbxTmGWanHmoWRXGn2p40hhAiDRFFvLotG04Iq6RlcAGL07HVTsMZSZihzdWhoNehFEIcd9jBvwJ-MXkQ3hwUQqmeled5fuSaPeTEulVOBXBO9hv02QpfDq125Mwcey80KvvHAwBh")

;; Base64URLデコード関数
(defun base64url-decode (string)
  "Base64URLエンコードされた文字列をデコード"
  (let* ((len (length string))
         (padding (mod (- 4 (mod len 4)) 4))
         (padded-string (concatenate 'string string (make-string padding :initial-element #\=)))
         ;; Base64URLからBase64への変換
         (base64-string (map 'string
                            (lambda (c)
                              (case c
                                (#\- #\+)
                                (#\_ #\/)
                                (otherwise c)))
                          padded-string)))
    (handler-case
        (cl-base64:base64-string-to-usb8-array base64-string)
      (error ()
        nil))))

;; 文字列分割ユーティリティ
(defun split-string (string delimiter)
  "文字列を指定した区切り文字で分割"
  (let ((result '())
        (start 0)
        (delimiter-char (char delimiter 0)))
    (loop for i from 0 below (length string)
            when (char= (char string i) delimiter-char)
          do (push (subseq string start i) result)
            (setf start (1+ i))
          finally (push (subseq string start) result))
    (nreverse result)))

;; alist（連想リスト）から値を取得するヘルパー関数
(defun get-value (key alist)
  "連想リストから指定したキーの値を取得"
  (cdr (assoc key alist :test #'string=)))

;; JWTのペイロード部分を取得してデコード
(defun extract-jwt-payload (token)
  "JWTトークンからペイロード部分を抽出してJSONとしてパース"
  (handler-case
      (let ((parts (split-string token ".")))
        (if (>= (length parts) 3)
            (let* ((payload-part (second parts))
                   (decoded-bytes (base64url-decode payload-part)))
              (if decoded-bytes
                  (let ((json-string (babel:octets-to-string decoded-bytes :encoding :utf-8)))
                    ;; jonathanは:as :alistオプションで連想リストとして返す
                    (jonathan:parse json-string :as :alist))
                  (error "Failed to decode base64url")))
            (error "Invalid JWT format - expected 3 parts separated by dots")))
    (error (e)
      (format t "Error extracting payload: ~A~%" e)
      nil)))

;; JWTヘッダーを取得してデコード
(defun extract-jwt-header (token)
  "JWTトークンからヘッダー部分を抽出"
  (handler-case
      (let ((parts (split-string token ".")))
        (if (>= (length parts) 3)
            (let* ((header-part (first parts))
                   (decoded-bytes (base64url-decode header-part)))
              (if decoded-bytes
                  (let ((json-string (babel:octets-to-string decoded-bytes :encoding :utf-8)))
                    (jonathan:parse json-string :as :alist))
                  (error "Failed to decode header")))
            (error "Invalid JWT format")))
    (error (e)
      (format t "Error extracting header: ~A~%" e)
      nil)))

;; Unix時間を人間が読める形式に変換
(defun unix-time-to-string (unix-time)
  "Unix時間を文字列に変換"
  (if unix-time
      (multiple-value-bind (sec min hour date month year)
          (decode-universal-time (+ unix-time 2208988800))
        (format nil "~4D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
          year month date hour min sec))
      "N/A"))

;; トークンの基本検証
(defun validate-token-claims (claims &optional project-id)
  "トークンのクレームを基本検証"
  (let ((now (- (get-universal-time) 2208988800))
        (exp (get-value "exp" claims))
        (iat (get-value "iat" claims))
        (iss (get-value "iss" claims))
        (aud (get-value "aud" claims)))

    (format t "Current time: ~A~%" (unix-time-to-string now))
    (format t "Token expires: ~A~%" (unix-time-to-string exp))
    (format t "Token issued: ~A~%" (unix-time-to-string iat))

    (cond
     ;; 有効期限チェック
     ((and exp (< exp now))
       (format t "❌ Token has expired~%")
       nil)

     ;; 発行時刻チェック
     ((and iat (> iat (+ now 300)))
       (format t "❌ Token issued in the future~%")
       nil)

     ;; 発行者チェック
     ((and project-id iss
           (not (string= iss (format nil "https://securetoken.google.com/~A" project-id))))
       (format t "❌ Invalid issuer: ~A~%" iss)
       nil)

     ;; オーディエンスチェック
     ((and project-id aud (not (string= aud project-id)))
       (format t "❌ Invalid audience: ~A~%" aud)
       nil)

     (t
       (format t "✅ Basic token validation passed~%")
       t))))

;; メイン認証関数
(defun authenticate-and-get-uid (token &optional project-id)
  "Firebaseトークンを認証してUIDを取得"
  (handler-case
      (progn
       (format t "=== Analyzing Firebase Token ===~%")

       ;; ヘッダー情報を表示
       (let ((header (extract-jwt-header token)))
         (when header
               (format t "Token algorithm: ~A~%" (get-value "alg" header))
               (format t "Key ID: ~A~%" (get-value "kid" header))))

       ;; ペイロード（クレーム）を取得
       (let ((claims (extract-jwt-payload token)))
         (if claims
             (progn
              (format t "~%=== Token Claims ===~%")

              ;; 主要な情報を表示
              (let ((uid (get-value "sub" claims))
                    (email (get-value "email" claims))
                    (email-verified (get-value "email_verified" claims))
                    (name (get-value "name" claims))
                    (picture (get-value "picture" claims))
                    (iss (get-value "iss" claims))
                    (aud (get-value "aud" claims)))

                (format t "UID (sub): ~A~%" uid)
                (format t "Email: ~A~%" email)
                (format t "Email verified: ~A~%" email-verified)
                (format t "Display name: ~A~%" name)
                (format t "Picture URL: ~A~%" picture)
                (format t "Issuer: ~A~%" iss)
                (format t "Audience: ~A~%" aud)

                ;; 基本検証を実行
                (format t "~%=== Validation ===~%")
                (if (validate-token-claims claims project-id)
                    (progn
                     (format t "~%✅ Authentication successful!~%")
                     (format t "Authenticated UID: ~A~%" uid)
                     (cons uid name))
                    (progn
                     (format t "~%❌ Token validation failed~%")
                     nil))))
             (progn
              (format t "❌ Failed to decode token payload~%")
              nil))))
    (error (e)
      (format t "Authentication error: ~A~%" e)
      nil)))

;; 簡略版（検証なしでUIDのみ取得）
(defun get-uid-simple (token)
  "トークンからUIDを簡単に取得（検証なし）"
  (let ((claims (extract-jwt-payload token)))
    (when claims
          (get-value "sub" claims))))

;; 全てのクレームを表示する関数
(defun show-all-claims (token)
  "トークン内の全クレームを表示"
  (let ((claims (extract-jwt-payload token)))
    (when claims
          (format t "=== All Token Claims ===~%")
          (dolist (claim claims)
            (format t "~A: ~A~%" (car claim) (cdr claim)))
          claims)))

;; 使用例
(defun demo-authentication ()
  "認証のデモ実行"
  (format t "=== Firebase Token Authentication Demo ===~%")
  (format t "Token length: ~A characters~%~%" (length *token*))

  ;; 完全な認証
  (format t "~%=== Full Authentication ===~%")
  (let ((uid (authenticate-and-get-uid *token* nil)))
    (if uid
        (format t "~%🎉 Successfully authenticated with UID: ~A~%" uid)
        (progn
         (format t "~%💥 Authentication failed~%")
         (format t "~%=== Simple UID Extraction ===~%")
         (let ((uid (get-uid-simple *token*)))
           (if uid
               (format t "UID extracted: ~A~%" uid)
               (format t "Failed to extract UID~%")))))))
