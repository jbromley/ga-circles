;;; -*- mode: lisp; indent-tabs: nil -*-
;;; ga-circles.lisp
;;; Use a genetic algorithm to find the largest possible circle that fits
;;; into a field of circles
;;;

;;; TODO Buckland: figure out if overlapping/out of bounds random circles are 
;;;      kept during initialization and then crossover/mutation
;;; TODO crossover two genes
;;; TODO mutate a gene
;;; TODO tournament selection of genes to cross
;;; TODO iterate generations

(in-package :ga-circles)

(defparameter +chromosome-length+ 28
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
  (max-x 1024)
  (max-y 1024))

(defun test ()
  (format t "Hello World from new project ga-circles~%")
  (let ((w (make-world)))
    (populate-world w)))


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
	 (< (+ x r) (world-max-x world))
	 (> (- y r) 0)
	 (< (+ y r) (world-max-y world)))))

(defun circle-no-overlap-p (circle world)
  "Returns T if CIRCLE is completely inside the bounds of WORLD and
does not overlap any other circle in WORLD. Returns NIL otherwise."
  (notany #'(lambda (world-circle) (circles-intersect-p world-circle circle))
	  (world-circles world)))

(defun populate-world (w)
  "Populate the world W with the number of circles determined by
+CIRCLE-POPULATION+."
  (do ((rs (make-random-state t)))
      ((= (length (world-circles w)) +circle-population+) w)
    (let ((c (make-circle :x (random (world-max-x w) rs)
			  :y (random (world-max-y w) rs)
			  :radius (random-radius))))
      ; Make sure circles are completely in the bounds of the world
      ; and do not overlap any other world circle.
      (when (and (circle-in-world-p c w)
		 (circle-no-overlap-p c w))
	(push c (world-circles w))))))
        

;;; Circle

(defun circle-area (circle)
  "Return the area of CIRCLE."
  (let ((r (circle-radius circle)))
    (* PI r r)))

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
  
;;; chromosomes

(defun random-chromosome (&optional (bit-length +chromosome-length+))
  "Return a random chromosome composed of BIT-LENGTH bits."
  (loop for i from 1 upto bit-length
       collect (random 2)))

(defun decode-chromosome (chromosome)
  (let ((x 0)
	(y 0)
	(r 0))
    (do ((i 0 (1+ i)))
	((= i 10))
      (setf x (+ x (* (nth i chromosome) (expt 2 i)))))
    (do ((i 0 (1+ i))
	 (elem 10 (1+ elem)))
	((= i 10))
      (setf y (+ y (* (nth elem chromosome) (expt 2 i)))))
    (do ((i 0 (1+ i))
	 (elem 20 (1+ elem)))
	((= i 8))
      (setf r (+ r (* (nth elem chromosome) (expt 2 i)))))
    (make-circle :x x :y y :radius r)))

(defun chromosome-fitness (world chromosome)
  (let ((circle (decode-chromosome chromosome)))
    (cond ((not (circle-in-world-p circle world)) 0)
	  ((not (circle-no-overlap-p circle world)) 0)
	  (t (circle-area circle)))))
	   