;;; -*- mode: lisp; indent-tabs: nil -*-

(defpackage :ga-circles
  (:use #:cl)
  (:export 
   #:circle
   #:make-circle
   #:circle-x
   #:circle-y
   #:circle-radius
   #:world
   #:world-circles
   #:world-x-max
   #:world-y-max
   #:world-bits
   #:test))
