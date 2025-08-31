(defpackage :converts.json
  (:use :cl :jonathan)
  (:export ))

(defun to-json (data)
  (jonathan:to-json data))

