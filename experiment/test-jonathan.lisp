(ql:quickload :jonathan)
(ql:quickload :flexi-streams)

;;; 関数定義

(format t "Parsed: ~A~%" (jonathan:parse "{\"key\":\"value\"" :junk-allowed t))

(defun safe-parse-json (json-string)
  "Parse JSON safely. Returns plist or :invalid. Logs input and result."
  (format t "~&[DEBUG] JSON input: ~A~%" json-string) ;; 入力ログ
  (handler-case
      (let ((result (jonathan:parse json-string
                                    :junk-allowed t)))
        (format t "~&[DEBUG] JSON parsed result: ~A~%" result) ;; 解析結果ログ
        result)
    (error (e)
      (format *error-output* "[ERROR] JSON parse error: ~A~%" e)
      :invalid)))

; ;;; テスト用 JSON
(defparameter *test-json* "{\"uid\":\"111111\",\"name\":\"Test User\",\"img\":\"test.png\"}")

; (let ((result (jonathan:parse *test-json* :junk-allowed t)))
;   (format t "~&Parsed plist: ~A~%" result))

; ;;; 実行
(format t "~&=== Running safe-parse-json test ===~%")
(let ((result (safe-parse-json *test-json*)))
  (format t "~&Parsed plist: ~A~%" result))
