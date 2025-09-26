; (defpackage :server-utils
;   (:use :cl :clack :websocket-driver :cl-dotenv :websocket-app)
;   (:export))

; (in-package :server-utils)

; (defvar *http-routes* (make-hash-table :test #'equal))
; (defvar *ws-routes* (make-hash-table :test #'equal))

; (defmacro defroute-http (path &body body)
;   `(setf (gethash ,path *http-routes*)
;      (lambda (env)
;        (let ((*env* env)) ;; env を動的変数として束縛
;          ,@body))))

; (defmacro defroute-ws (path &body body)
;   `(setf (gethash ,path *ws-routes*)
;      (lambda (env)
;        (let ((ws (make-server env)))
;          ,@body
;          (lambda (responder)
;            (declare (ignore responder))
;            (start-connection ws))))))

; (defun validate-service-token (env)
;   "env から X-Service-Token を取り出し、*backend-token-secret* と一致するか検証する。
; ヘッダがハッシュテーブル／plist／alist のいずれでも動作するようにする。"
;   (let* ((headers (getf env :headers))
;          (find-in-htable
;           (lambda (ht key)
;             (let ((found nil))
;               (maphash (lambda (k v)
;                          (when (and (stringp (string k))
;                                     (string= (string-downcase (string k))
;                                              (string-downcase key)))
;                                (setf found v)))
;                        ht)
;               found)))
;          (token
;           (cond
;            ((typep headers 'hash-table)
;              (funcall find-in-htable headers "x-service-token"))
;            ((listp headers)
;              (or (getf headers :x-service-token)
;                  (getf headers :X-SERVICE-TOKEN)
;                  (let ((pair (assoc "x-service-token" headers :test
;                                     (lambda (a b)
;                                       (and (stringp a)
;                                            (string= (string-downcase a)
;                                                     (string-downcase b)))))))
;                    (when pair (cdr pair)))))
;            (t nil))))
;     (and token (string= token utils-env:*backend-token-secret*))))


; (defun with-cors (handler)
;   (lambda (env)
;     (multiple-value-bind (status headers body)
;         (funcall handler env)
;       (values status
;         (append
;           '(("Access-Control-Allow-Origin" . "http://localhost:3000/")
;             ("Access-Control-Allow-Methods" . "GET, POST, PUT, DELETE, OPTIONS")
;             ("Access-Control-Allow-Headers" . "Content-Type, Authorization"))
;           headers)
;         body))))
