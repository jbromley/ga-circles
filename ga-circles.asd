;;; -*- mode: lisp; indent-tabs: nil -*-

(defsystem :ga-circles
  :name "ga-circles"
  :author "J. Bromley <jbromley@gmail.com>"
  :version "0.1"
  :description "Circle packing with a genetic algorithm"

  :serial t
  ;; add new files to this list:
  :components ((:file "package") 
	       (:file "ga-circles"))
  :depends-on (lispbuilder-sdl))
