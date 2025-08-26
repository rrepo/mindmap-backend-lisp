(ql:quickload :postmodern)
(use-package :postmodern)
(ql:quickload :split-sequence)
(use-package :split-sequence)

(defparameter *db* (connect "mindmap" "postgres" "password" "localhost"))

(setf *database* *db*)

(defpackage :init-db-utils
  (:use :cl :postmodern :split-sequence)
  (:export
   init-db))

(in-package :init-db-utils)

(defun load-sql-file (file-path)
  "指定したSQLファイルを文字列として読み込む。"
  (with-open-file (in file-path
                      :direction :input
                      :external-format :utf-8)
    (let ((contents (make-string (file-length in))))
      (read-sequence contents in)
      contents)))

(defun execute-sql-file (file-path &key split-statements)
  "SQLファイルを読み込んで実行する。
  split-statements が T のときはセミコロンで分割して一文ずつ execute。
  split-statements が NIL のときはファイル全体をそのまま execute する。"
  (let ((sql-content (load-sql-file file-path)))
    (if split-statements
        ;; セミコロンで分割し一文ずつ実行
        (dolist (stmt
                 (remove-if #'(lambda (s) (string= s ""))
                   (mapcar (lambda (s)
                             (string-trim " \t\n" s))
                       (split-sequence:split-sequence #\; sql-content))))
          (when (> (length stmt) 0)
                (format t "~%Executing SQL: ~A~%" stmt)
                (postmodern:execute stmt)))
        ;; 分割せず丸ごと実行
        (progn
         (format t "~%Executing SQL file as single statement: ~A~%" file-path)
         (postmodern:execute sql-content)))))

; (execute-sql-file "./create-tables.sql" :split-statements t)
; (execute-sql-file "./trigger-set.sql" :split-statements nil)
; (execute-sql-file "./trigger-each.sql" :split-statements t)

(defun init-db ()
  "データベースを初期化する。"
  (format t "Initializing database...~%")
  ;; テーブル作成
  (execute-sql-file "./models/create-tables.sql" :split-statements t)
  ;; トリガー定義
  (execute-sql-file "./models/trigger-set.sql" :split-statements nil)
  ;; 各トリガー作成
  (execute-sql-file "./models/trigger-each.sql" :split-statements t)
  (format t "Database initialization complete.~%"))