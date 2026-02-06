(in-package :ws-utils)

;;; ---------------------------
;;; close handler
;;; ---------------------------
(defun ws-close-handler (ws)
  (let ((client (gethash ws websocket-app:*ws-clients*)))
    (when client
          (maphash
            (lambda (target _uuid)
              (let ((bucket (gethash target websocket-app:*subscriptions*)))
                (when bucket
                      (remhash ws bucket)
                      (when (zerop (hash-table-count bucket))
                            (remhash target websocket-app:*subscriptions*)))))
            (getf client :subscriptions)))
  (remhash ws websocket-app:*ws-clients*)
    (format t "[WS] Closed: ~A~%" ws)))

;;; ---------------------------
;;; SUBSCRIBE / UNSUBSCRIBE / BROADCAST
;;; ---------------------------
(defun ws-subscribe (ws target)
  (format t "[WS] SUBSCRIBE ws=~A target=~A~%" ws target)

  (let* ((client (gethash ws websocket-app:*ws-clients*))
         (subs (getf client :subscriptions))
         (bucket (or (gethash target websocket-app:*subscriptions*)
                     (setf (gethash target websocket-app:*subscriptions*)
                       (make-hash-table :test 'equal)))))
    (setf (gethash ws bucket) t)
    (setf (gethash target subs) t)

    (format t "[WS] bucket-count=~A~%"
      (hash-table-count bucket))

    (websocket-driver:send ws (jonathan:to-json
              `(:type "SUBSCRIBED"
                      :target ,target)))))

(defun ws-unsubscribe (ws target)
  "WS を target の bucket から削除し client subscriptions も削除"
  (let* ((client (gethash ws websocket-app:*ws-clients*))
         (subs (getf client :subscriptions))
         (bucket (gethash target websocket-app:*subscriptions*)))
    (when bucket
          (remhash ws bucket)
          (when (zerop (hash-table-count bucket))
                (remhash target websocket-app:*subscriptions*)))
    (when subs
          (remhash target subs))
    ;; UNSUBSCRIBED メッセージ
    (websocket-driver:send ws (jonathan:to-json
              `(:type "UNSUBSCRIBED"
                      :target ,target)))))

(defun ws-broadcast-to-target (target message)
  "target に登録されているクライアントに broadcast"
  (let ((bucket (gethash target websocket-app:*subscriptions*)))
    (when bucket
          (maphash
            (lambda (ws _uuid)
              (ignore-errors
                (websocket-driver:send ws message)))
            bucket))))

(defun ws-broadcast (message)
  "全クライアントに broadcast"
  (maphash
    (lambda (_ ws-client)
      (ignore-errors
        (websocket-driver:send (getf ws-client :ws) message)))
    websocket-app:*ws-clients*))
