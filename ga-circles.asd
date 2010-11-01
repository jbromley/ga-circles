;;; -*- mode: lisp; indent-tabs: nil -*-

(defsystem :ga-circles
  :serial t
  ;; add new files to this list:
  :components ((:file "package") (:file "ga-circles"))
  :depends-on (#+nil :lispbuilder-sdl))
