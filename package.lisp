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
   #:world-max-x
   #:world-max-y
   #:world-bits
   #:make-world
   #:populate-world
   #:random-chromosome
   #:decode-chromosome
   #:mutate-chromosome
   #:chromosome-fitness
   #:create-population
   #:test))
