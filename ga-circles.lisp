;;; -*- mode: lisp; indent-tabs: nil -*-
;;; ga-circles.lisp
;;; Use a genetic algorithm to find the largest possible circle that fits
;;; into a field of circles
;;;
;;; TODO create random gene
;;; TODO decode a gene into a circle
;;; TODO crossover two genes
;;; TODO mutate a gene
;;; TODO calculate fitness of a gene
;;; TODO tournament selection of genes to cross
;;; TODO iterate generations

(in-package :ga-circles)

(defparameter +gene-length+ 10 
  "Number of bits used to represent each characteristic")

(defparameter +circle-population+ 50
  "Number of circles in the environment.")

(defparameter +circle-min-radius+ 4
  "Minimum radius of circle in world.")

(defparameter +circle-max-radius+ 128
  "Maximum radius of circle in world")

(defstruct circle
  x
  y
  radius)

(defstruct world
  (circles ())
  (x-max (expt 2 +gene-length+))
  (y-max (expt 2 +gene-length+))
  (bits +gene-length+))

(defun test ()
  (format t "Hello World from new project ga-circles~%")
  (let ((w (make-world)))
    (populate-world w)
    (display-world w)))


;;; World

(defun random-radius (&optional (random-state *random-state*))
  "Return a random number between the minimum circle radius and
maximum circle radius."
  (+ +circle-min-radius+ (random 
			  (- +circle-max-radius+ +circle-min-radius+)
			  random-state)))

(defun circle-in-world-p (circle world)
  "Determine if the circle CIRCLE is completely within the bounds of the 
WORLD environment."
  (let ((x (circle-x circle))
	(y (circle-y circle))
	(r (circle-radius circle)))
    (and (> (- x r) 0)
	 (< (+ x r) (world-x-max world))
	 (> (- y r) 0)
	 (< (+ y r) (world-y-max world)))))

(defun populate-world (w)
  "Populate the world W with the number of circles determined by
+CIRCLE-POPULATION+."
  (do ((rs (make-random-state t)))
      ((= (length (world-circles w)) +circle-population+) w)
    (let ((c (make-circle :x (random (world-x-max w) rs)
			  :y (random (world-y-max w) rs)
			  :radius (random-radius))))
      ; Make sure circles are completely in the bounds of the world
      ; and do not overlap any other world circle.
      (when (and (circle-in-world-p c w)
		 (notany #'(lambda (wc) (circles-intersect-p wc c))
			 (world-circles w)))
	(push c (world-circles w))))))
        

;;; Circle

(defun circle-distance-squared (circle1 circle2)
  "Returns the square of the distance between the centers of two circles."
  (let ((dx (- (circle-x circle1) (circle-x circle2)))
	(dy (- (circle-y circle1) (circle-y circle2))))
    (+ (* dx dx) (* dy dy))))

(defun circle-distance (circle1 circle2)
  "Returns the distance between the centers of two circles."
  (sqrt (circle-distance-squared circle1 circle2)))
  
(defun circles-intersect-p (circle1 circle2)
  "Determines if two circles intersect."
  (<= (circle-distance circle1 circle2)
     (+ (circle-radius circle1) (circle-radius circle2))))
  

;;; Graphics

(defun display-world (world)
  (sdl:with-init ()
    (sdl:window (world-x-max world) (world-y-max world)
		:title-caption "GA Circles"
		:icon-caption "GA Circles")
    (setf (sdl:frame-rate) 0)
    (dolist (c (world-circles world))
      (sdl:draw-filled-circle-* (circle-x c) (circle-y c) (circle-radius c)
				:color (sdl:color :r 0 :g 0 :b 192 :a 128) 
				:alpha 255
				:surface sdl:*default-display*))
    (sdl:update-display)
    (sdl:with-events ()
      (:quit-event () t)
      (:video-expose-event () (sdl:update-display)))))
		      

