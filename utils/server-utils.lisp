(defpackage :server-utils
  (:use :cl :clack :websocket-driver :cl-dotenv)
  (:export))

(in-package :server-utils)

(defmacro defroute-http (path &body body)
  `(setf (gethash ,path *http-routes*)
     (lambda (env) ,@body)))

(defmacro defroute-ws (path &body body)
  `(setf (gethash ,path *ws-routes*)
     (lambda (env)
       (let ((ws (make-server env)))
         ,@body
         (lambda (responder)
           (declare (ignore responder))
           (start-connection ws))))))

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
    (and token (string= token *backend-token-secret*))))