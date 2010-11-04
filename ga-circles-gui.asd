;;; -*- mode: lisp; indent-tabs: nil -*-

(defsystem ga-circles-gui
  :description "GUI programs for GA circles."
  :author "J. Bromley <jbromley@gmail.com>"
  :version "0.1"
  :depends-on (ga-circles lispbuilder-sdl)
  :components
  ((:module "gui"
    :components
    ((:file "package")
     (:file "gui" :depends-on ("package"))))))
  