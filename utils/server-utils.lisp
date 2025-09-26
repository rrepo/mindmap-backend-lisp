(defpackage :server-utils
  (:use :cl)
  (:export validate-service-token with-api-response))

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