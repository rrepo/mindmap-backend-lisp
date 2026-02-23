(load "~/quicklisp/setup.lisp")

(ql:quickload :mindmap)

(sb-ext:save-lisp-and-die
 "mindmap"
 :toplevel (lambda ()
             (funcall
              (symbol-function
               (find-symbol "MAIN" "MINDMAP"))))
 :executable t)