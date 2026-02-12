(defpackage :server-utils
  (:use :cl)
  (:export validate-service-token with-api-response validate-ws-cookie get-cookie-value))

(in-package :server-utils)

(defun validate-service-token (env)
  "env から X-Service-Token を取り出し、*backend-token-secret* と一致するか検証する。
ヘッダがハッシュテーブル／plist／alist のいずれでも動作するようにする。"
  (let* ((headers (getf env :headers))
         (find-in-htable
          (lambda (ht key)
            (let ((found nil))
              (maphash (lambda (k v)
                         (when (and (stringp (string k))
                                    (string= (string-downcase (string k))
                                             (string-downcase key)))
                               (setf found v)))
                       ht)
              found)))
         (token
          (cond
           ((typep headers 'hash-table)
             (funcall find-in-htable headers "x-service-token"))
           ((listp headers)
             (or (getf headers :x-service-token)
                 (getf headers :X-SERVICE-TOKEN)
                 (let ((pair (assoc "x-service-token" headers :test
                                    (lambda (a b)
                                      (and (stringp a)
                                           (string= (string-downcase a)
                                                    (string-downcase b)))))))
                   (when pair (cdr pair)))))
           (t nil))))
    (and token (string= token utils-env:*backend-token-secret*))))

(defun extract-cookie-value (cookie name)
  "Cookie ヘッダ文字列から name の値を取り出す"
  (when cookie
        (let* ((key (concatenate 'string name "="))
               (start (search key cookie :test #'char-equal)))
          (when start
                (let* ((value-start (+ start (length key)))
                       (end (or (position #\; cookie :start value-start)
                                (length cookie))))
                  (subseq cookie value-start end))))))

(defun base64ish-p (s)
  "Base64 / Base64URL として使えそうな文字列か簡易チェック"
  (and (stringp s)
       (> (length s) 0)
       (every (lambda (c)
                (or (alphanumericp c)
                    (find c "+/=_-" :test #'char=)))
           s)))

(defun validate-ws-cookie (env)
  (let* ((headers (getf env :headers))
         (cookie
          (cond
           ((typep headers 'hash-table)
             (let ((found nil))
               (maphash
                 (lambda (k v)
                   (when (string-equal (string-downcase (string k)) "cookie")
                         (setf found v)))
                 headers)
               found))
           ((listp headers)
             (or (getf headers :cookie)
                 (cdr (assoc "cookie" headers :test #'string-equal))))
           (t nil)))
         ;; ★ ここを修正
         (token (extract-cookie-value cookie "ws-token")))
    (format t "[ws-auth] raw Cookie header = ~a~%" cookie)
    (format t "[ws-auth] headers = ~s~%" headers)
    (and token
         (base64ish-p token)
         (string= token utils-env:*ws-token-secret*))))


(defmacro with-api-response (result)
  `(let ((res ,result))
     (cond
      ((null res) ;; データなし
                 `(200 (:content-type "application/json")
                       (,(jonathan:to-json '(:status "success" :data ())))))
      ((eq res :invalid)
        `(400 (:content-type "application/json")
              (,(jonathan:to-json '(:status "error")))))
      (t
        `(200 (:content-type "application/json")
              (,(jonathan:to-json
                 (list :status "success" :data res))))))))

(defun with-cors (handler)
  (lambda (env)
    (multiple-value-bind (status headers body)
        (funcall handler env)
      (values status
        (append
          '(("Access-Control-Allow-Origin" . "http://localhost:3000/")
            ("Access-Control-Allow-Methods" . "GET, POST, PUT, DELETE, OPTIONS")
            ("Access-Control-Allow-Headers" . "Content-Type, Authorization"))
          headers)
        body))))

(defun get-cookie-value (cookie-header name)
  "Cookie ヘッダーから name の値を取り出す"
  (when cookie-header
        (some (lambda (pair)
                (when (string= (string-trim " " (subseq pair 0 (position #\= pair)))
                               name)
                      (string-trim " " (subseq pair (+ 1 (position #\= pair))))))
            (cl-ppcre:split "; " cookie-header))))
