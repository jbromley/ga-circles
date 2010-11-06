;;; -*- mode: lisp; indent-tabs: nil -*-

(asdf:defsystem #:ga-circles
  :name "ga-circles"
  :author "J. Bromley <jbromley@gmail.com>"
  :version "0.1"
  :description "Circle packing with a genetic algorithm"
  :depends-on ()
  :serial t
  :components ((:file "package") 
	       (:file "ga-circles" :depends-on ("package"))))

