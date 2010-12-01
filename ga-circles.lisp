;;; -*- mode: lisp; indent-tabs: nil -*-
;;; ga-circles.lisp
;;; Use a genetic algorithm to find the largest possible circle that fits
;;; into a field of circles
;;;
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
  (size 0)
  (members '())
  (total-fitness 0)
  (best-fitness 0)
  (fitness-sums 0)
  (generation 0)
  (elites 0))

(defun run (&key (circles 16) (pop-size 32) (iterations 100) (elites 2))
  (format t "ga-circles~%")
  (let ((w (create-world circles)))
    (do ((i 0 (1+ i))
	 (p (create-initial-population w :n pop-size :elites elites)
	    (next-generation w p)))
	((= i iterations) p))))


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
    (and (> x r)
	 (< (+ x r) (world-max-x world))
	 (> y r)
	 (< (+ y r) (world-max-y world)))))

(defun circle-no-overlap-p (circle world)
  "Returns T if CIRCLE is completely inside the bounds of WORLD and
does not overlap any other circle in WORLD. Returns NIL otherwise."
  (notany #'(lambda (world-circle) (circles-intersect-p world-circle circle))
	  (world-circles world)))

(defun create-world (&optional (circles +world-circles+))
  "Create a world and populate it with N circles. If N is not specified then
+WORLD-CIRCLES+ circles are created."
  (let ((w (make-world)))
    (populate-world w circles)))

(defun populate-world (w &optional (n +world-circles+))
  "Populate the world W with N circles. If N is not specified then 
+WORLD-CIRCLES+ circles are created."
  (do ((rs (make-random-state t)))
      ((= (length (world-circles w)) n) w)
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

(defun distance (x1 y1 x2 y2)
  (let ((dx (- x2 x1))
	(dy (- y2 y1)))
    (sqrt (+ (* dx dx) (* dy dy)))))

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
  (let ((d-squared (circle-distance-squared circle1 circle2))
	(max-d (+ (circle-radius circle1) (circle-radius circle2))))
  (<= d-squared (* max-d max-d))))

  
;;; chromosomes

(defun create-seq (start end)
  "Creates a list with the numbers from START to END inclusive."
  (loop for i from start upto end collect i))

(defun create-random-chromosome (&optional (bit-length +chromosome-length+))
  "Returns a random chromosome composed of BIT-LENGTH bits."
  (loop for i from 1 upto bit-length collect (random 2)))

(defun bits-to-integer (bits &optional (accum 0) (power 1))
  "Converts a string of bits represented as integer 1 and 0 in a list
into an integer number. The least significant bit comes first in the
bit sequence."
  (if (null bits) 
      accum
      (bits-to-integer (rest bits) (+ accum (* power (first bits)))
		       (* 2 power))))

(defun gray-to-binary (bits &optional (accum '()) xor-bit)
  "Converts a string of BITS represented as integer 1 and 0 in a list
from Gray code to an integer. ACCUM holds the current result and
XOR-BIT is the result of the xor of all bits up to the current bit."
  (cond ((null bits) (reverse accum))
	((endp accum) 
	 (gray-to-binary (rest bits) (push (first bits) accum) (first bits)))
	(t (gray-to-binary (rest bits) 
			   (push (logxor xor-bit (first bits)) accum)
			   (first accum)))))

(defun decode-chromosome (chromosome)
  "Decodes CHROMOSOME into the circle represented by the
chromosome. The x-coordinate is bits 0 through 9, the y-coordinate is
bits 10 through 19 and the radius is bits 20 through the end."
  (let ((x (bits-to-integer (gray-to-binary (subseq chromosome 0 10))))
	(y (bits-to-integer (gray-to-binary (subseq chromosome 10 20))))
	(r (bits-to-integer (gray-to-binary (subseq chromosome 20)))))
    (make-circle :x x :y y :radius r)))

(defun mutate-chromosome (chromosome &optional (mutation-rate +mutation-rate+))
  "Randomly mutates the bit sequence CHROMOSOME. The probability of a
single bit flip is MUTATION-RATE."
  (mapcar #'(lambda (bit) (if (< (random 100) mutation-rate) 
			      (mod (+ bit 1) 2)
			      bit))
	  chromosome))

(defun choose-crossover-points (num-pts chromo-len)
    (do ((xover-pts '())
	 (pt (1+ (random (1- chromo-len))) (1+ (random (1- chromo-len)))))
	((= (length xover-pts) num-pts) (sort xover-pts #'<))
      (when (not (member pt xover-pts))
	(push pt xover-pts))))

(defun crossover-chromosomes (parents xover-pts 
			      &optional (prob +crossover-rate+))
  "Cross over the two parent chromosomes in PARENTS at the points
indicated by XOVER-PTS with probability PROB."
  (if (< (random 100) prob)
      (let ((offspring-1 '())
	    (offspring-2 '()))
	(do* 
	 ((index 0 (1+ index))
	  (start 0 end)
	  (end (nth index xover-pts) (nth index xover-pts)))
	 ((> index (length xover-pts)) 
	  (list (reverse offspring-1) (reverse offspring-2)))
	  (dolist (elem (subseq (nth (mod index 2) parents) start end))
	    (push elem offspring-1))
	  (dolist (elem (subseq (nth (mod (1+ index) 2) parents) start end))
	    (push elem offspring-2))))
      parents))

(defun chromosome-fitness (world chromosome)
  (let ((circle (decode-chromosome chromosome)))
    (cond ((not (circle-in-world-p circle world)) 0)
	  ((not (circle-no-overlap-p circle world)) 0)
	  (t (circle-radius circle)))))

(defun create-chromosomes (&optional (n +circle-population+))
  "Create a population of N chromosomes."
  (let ((population ()))
    (dotimes (i n)
      (push (create-random-chromosome) population))
    population))

(defun create-fitter-chromosomes (world &optional (n +circle-population+))
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
  (flet ((calc-fitness (chromo) (chromosome-fitness world chromo)))
    (let* ((init-population (create-fitter-chromosomes world n))
	   (fitness-list (sort (mapcar #'calc-fitness init-population) #'>)))
      (make-population
       :size n
       :members (sort init-population #'> :key #'calc-fitness)
       :total-fitness (reduce #'+ fitness-list)
       :best-fitness (apply #'max fitness-list)
       :fitness-sums (loop for rank from n downto 1
			summing rank into total collect total)
       ;; :fitness-sums (loop for elem in fitness-list
       ;; 			summing elem into total collect total)
       :elites elites))))
   
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
    (unless (member candidate selected)
      (push candidate selected))))

(defun next-generation (world population)
  "Iterate the next generation of POPULATION in the environment WORLD."
  (flet ((calc-fitness (chromo) (chromosome-fitness world chromo)))
    (let ((members (population-members population))
	  (n (population-size population))
	  (elites (population-elites population))
	  (next-gen '()))
      ; First do elitism.
      (let* ((viable (count-if #'(lambda (fitness) (not (zerop fitness))) 
			       members :key #'calc-fitness))
	     (num-elites (min viable elites)))
	(dolist (chromo (subseq members 0 num-elites)) (push chromo next-gen)))
	; Now do crossovers until the new generation is full.
    (do ((parents (roulette-select-pair population) 
		  (roulette-select-pair population))
	 (xovers (choose-crossover-points 2 (length (first members)))
		 (choose-crossover-points 2 (length (first members)))))
	((= (population-size population) (length next-gen)))
      (let ((offspring (mapcar #'mutate-chromosome 
			       (crossover-chromosomes parents xovers))))
	(dolist (chromo offspring) (push chromo next-gen))))
    ; Calulate statistics and create the new population.
    (let ((fitness-list (sort (mapcar #'calc-fitness next-gen) #'>)))
      (make-population
       :size n
       :members (sort next-gen #'> :key #'calc-fitness)
       :total-fitness (reduce #'+ fitness-list)
       :best-fitness (apply #'max fitness-list)
       ;; :fitness-sums (loop for fitness in fitness-list
       ;; 			summing fitness into total collect total)
       :fitness-sums (loop for rank from n downto 1
			summing rank into total collect total)
       :generation (1+ (population-generation population))
       :elites elites)))))

(defun population-most-fit (population &optional (n 1))
  "Return the N most fit members of POPULATION."
  (subseq (population-members population) 0 n))

(defun population-most-fit-circles (population &optional (n 1))
  "Return the decoded form (circle) of the N most fit members of POPULATION." 
  (mapcar #'decode-chromosome (population-most-fit population n)))

(defun find-viable (world population)
  "Return a list of all viable (fitness greater than 0) members of POPULATION."
  (remove-if-not #'(lambda (chromo) (> (chromosome-fitness world chromo) 0))
		 (population-members population)))

