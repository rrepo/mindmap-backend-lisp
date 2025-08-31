(ql:quickload :jonathan)
(use-package :jonathan)

(let* ((json-string "{\"uid\":\"u123\",\"name\":\"Taro\",\"img\":\"http://example.com\"}")
       (data (jonathan:parse json-string :keywordize t)))
  (print data))
