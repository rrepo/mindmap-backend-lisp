(defpackage :controllers.mindmaps
  (:use :cl :jonathan)
  (:export get-all-maps))

(load "./models/maps.lisp")
(load "./utils/utils.lisp")

(in-package :controllers.mindmaps)

(defun get-all-maps ()
  (format *error-output* "Controller: get-all-maps call!!!!ed~%")
  (utils:with-invalid
   (let* ((maps (models.maps:get-all-maps)))
     (format *error-output* "Maps data: ~A~%" maps)
     maps)))
