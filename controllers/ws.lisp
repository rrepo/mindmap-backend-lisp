(in-package :controllers.ws)

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

(defun ws-on-open (ws)
  (setf (gethash ws websocket-app:*ws-clients*)
    (list :ws ws
          :subscriptions (make-hash-table :test 'equal)))
  (format t "[WS] OPEN: ~A~%" ws))


(defun ws-on-message (ws msg)
  (format t "[WS] RAW MESSAGE: ~A~%" msg)
  (handler-case
      (let* ((data (jonathan:parse msg :as :hash-table))
             (type (string-upcase (gethash "type" data)))
             (target (gethash "target" data)))
        (cond
         ((string= type "SUBSCRIBE")
           (ws-utils:ws-subscribe ws target))
         ((string= type "UNSUBSCRIBE")
           (ws-utils:ws-unsubscribe ws target))
         (t
           (format t "[WS] Unknown type: ~A~%" type))))
    (error (e)
      (format *error-output* "[WS ERROR] ~A~%" e))))


(defun ws-on-close (ws)
  (ws-utils:ws-close-handler ws))
