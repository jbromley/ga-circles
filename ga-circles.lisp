;;; -*- mode: lisp; indent-tabs: nil -*-
;;; ga-circles.lisp
;;; Use a genetic algorithm to find the largest possible circle that fits
;;; into a field of circles
;;;

;;; TODO iterate generations
;;; TODO integrate with ga-circles-gui to display evolution in real-time
;;; TODO generalize crossover-chromosomes to take more than one crossover point
;;; TODO rewrite to be more functional

(in-package :ga-circles)

(defparameter +chromosome-length+ 29
  "Number of bits used to represent each characteristic")

(defparameter +world-circles+ 50
  "Number of fixed circles in the world.")

(defparameter +circle-population+ 100
  "Number of circles in the evolving population.")

(defparameter +crossover-rate+ 80
  "Probability (in percent) of crossover occurring.")

(defparameter +mutation-rate+ 5
  "Probability (in percent) of a single-bit mutation.")

(defparameter +circle-min-radius+ 16
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

(defstruct population
  (members '())
  (total-fitness 0)
  (best-fitness 0)
  (fitness-sums 0)
  (generation 0)
  (elites 0))

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
+WORLD-CIRCLES+."
  (do ((rs (make-random-state t)))
      ((= (length (world-circles w)) +world-circles+) w)
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

(defun create-seq (start end)
  "Create a list with the numbers from START to END inclusive."
  (loop for i from start upto end collect i))

(defun create-random-chromosome (&optional (bit-length +chromosome-length+))
  "Return a random chromosome composed of BIT-LENGTH bits."
  (loop for i from 1 upto bit-length collect (random 2)))

(defun bits-to-integer (bits)
  "Convert a string of bits represented as integer 1 and 0 in a list
into an integer number. The least significant bit comes first in the
bit sequence."
  (let ((powers (create-seq 0 (1- (length bits)))))
    (reduce #'+ (mapcar #'(lambda (bit power) (* bit (expt 2 power)))
			bits powers))))

(defun decode-chromosome (chromosome)
  "Decodes CHROMOSOME into the circle represented by the
chromosome. The x-coordinate is bits 0 through 9, the y-coordinate is
bits 10 through 19 and the radius is bits 20 through the end."
  (let ((x (bits-to-integer (subseq chromosome 0 10)))
	(y (bits-to-integer (subseq chromosome 10 20)))
	(r (bits-to-integer (subseq chromosome 20))))
    (make-circle :x x :y y :radius r)))

(defun mutate-chromosome (chromosome &optional (mutation-rate +mutation-rate+))
  "Randomly mutates the bit sequence CHROMOSOME. The probability of a
single bit flip is MUTATION-RATE."
  (mapcar #'(lambda (bit) (if (< (random 100) mutation-rate) 
			      (mod (+ bit 1) 2)
			      bit)) 
	  chromosome))

(defun crossover-chromosomes (chromo-1 chromo-2 x)
  "Does a single-point crossover of CHROMO-1 and CHROMO-2. The
crossover point as after X bits."
	      (list (append (subseq chromo-1 0 x) (subseq chromo-2 x))
		    (append (subseq chromo-2 0 x) (subseq chromo-1 x))))

(defun chromosome-fitness (world chromosome)
  (let ((circle (decode-chromosome chromosome)))
    (cond ((not (circle-in-world-p circle world)) 0)
	  ((not (circle-no-overlap-p circle world)) 0)
	  (t (circle-radius circle)))))

(defun create-population (&optional (n +circle-population+))
  "Create a population of N chromosomes."
  (let ((population ()))
    (dotimes (i n)
      (push (create-random-chromosome) population))
    population))

(defun create-fitter-population (world &optional (n +circle-population+))
  "Create a population of N chromosomes. Ensure that no chromosome has
zero fitness."
  (let ((population '()))
    (do ((chromo (create-random-chromosome) (create-random-chromosome)))
	((= (length population) n) population)
      (when (> (chromosome-fitness world chromo) 0)
	(push chromo population)))))

(defun create-initial-population (world &key (n +circle-population+) (elites 0))
  "Creates an initial population of N chromosomes to live in the
environment WORLD. Epochs will maintain ELITES most fit members. The
population is sorted by fitness and the best and total fitness are
calculated."
  (let* ((init-population (create-fitter-population world n))
	 (fitness-list 
	  (sort (mapcar #'(lambda (chromo) (chromosome-fitness world chromo)) 
		  init-population) #'>)))
  (make-population
   :members (sort init-population #'> 
		  :key #'(lambda (chromo) (chromosome-fitness world chromo)))
   :total-fitness (reduce #'+ fitness-list)
   :best-fitness (apply #'max fitness-list)
   :fitness-sums (loop for elem in fitness-list
		    summing elem into total collect total)
   :elites elites)))
   
(defun roulette-select (population)
  (let* ((sums (population-fitness-sums population))
	 (sel (random (apply #'max sums))))
    (loop for elem in sums with index = 0
	 do (when (> elem sel) 
	      (return (nth index (population-members population))))
	 (incf index))))

(defun roulette-select-pair (population)
  "Select two members of POPULATION in the environment WORLD via
roulette-wheel selection."
  (do ((selected '())
       (candidate (roulette-select population) (roulette-select population)))
      ((= (length selected) 2) selected)
    (when (not (member candidate selected)) 
      (push candidate selected))))
      

  