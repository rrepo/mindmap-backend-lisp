(load "~/quicklisp/setup.lisp")

(ql:quickload :mindmap)

;; ここで main を確定させる
(let ((main-fn (symbol-function (find-symbol "MAIN" "MINDMAP"))))
  (sb-ext:save-lisp-and-die
   "mindmap"
   :toplevel main-fn
   :executable t))