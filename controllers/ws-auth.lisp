(defpackage :controllers.ws-auth
  (:use :cl)
  (:export handle-ws-token-http-cookie))

(in-package :controllers.ws-auth)

(defun handle-ws-token-http-cookie (env)
  (let* ((token utils-env:*ws-token-secret*)
         (cookie (format nil
                     "ws-token=~a; HttpOnly; Path=/; SameSite=Lax"
                   token))
         (body (babel:string-to-octets "OK" :encoding :utf-8)))
    ;; logs
    (format t "[ws-auth] issue ws-token cookie~%")
    (format t "[ws-auth] token=~a~%" token)
    (format t "[ws-auth] Set-Cookie=~a~%" cookie)
    (finish-output)

    `(200
      (:headers (("Content-Type" . "text/plain")
                 ("Set-Cookie" . ,cookie)))
      ,body)))


; (let ((cookie (format nil
;   "ws-token=~a; HttpOnly; Secure; Path=/; SameSite=Lax"
;   token)))