(in-package #:ga-circles-gui)

(defparameter +world-color+ (sdl:color :r 0 :g 0 :b 128 :a 64)
  "The color of the interior of a world circle.")

(defparameter +world-outline-color+ (sdl:color :r 0 :g 0 :b 255 :a 255)
  "The color of the outline of a world circle.")

(defparameter +best-color+ (sdl:color :r 0 :g 128 :b 0 :a 128)
  "The color of the interior of the best circle in the population.")

(defparameter +best-outline-color+ (sdl:color :r 0 :g 255 :b 0 :a 255)
  "The color of the outline of the best circle in the population.")

(defparameter +default-color+ (sdl:color :r 128 :g 128 :b 128 :a 128)
  "The color of the outline of non-optimal circles in the population.")



(defun draw-outlined-circle (c outline-color interior-color)
  "Draw a circle C with the outline in color OUTLINE-COLOR and the
interior of the circle in color INTERIOR-COLOR."
  (sdl:draw-circle-* (circle-x c) (circle-y c) (circle-radius c)
		     :color outline-color :alpha 255
		     :surface sdl:*default-display*)      
  (sdl:draw-filled-circle-* (circle-x c) (circle-y c) (circle-radius c)
			    :color interior-color :alpha 255
			    :surface sdl:*default-display*))

(defun draw-circle (c color)
  "Draw the outline of the circle C using COLOR as the color."
  (sdl:draw-circle-* (circle-x c) (circle-y c) (circle-radius c)
		     :color color :alpha 255
		     :surface sdl:*default-display*))
  
(defun display-world (world)
  "Draw circles in the environment WORLD onto an SDL surface."
  (sdl:with-init ()
    (sdl:window (world-max-x world) (world-max-y world)
		:title-caption "GA Circles (World)"
		:icon-caption "GA Circles (World)")
    (setf (sdl:frame-rate) 0)
    (dolist (c (world-circles world))
      (draw-outlined-circle c +world-outline-color+ +world-color+))
    (sdl:update-display)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event ()
		       (when (sdl:key-down-p :sdl-key-escape)
			 (sdl:push-quit-event)))
      (:video-expose-event () (sdl:update-display)))))

(defun run (&key (circles 50) (pop-size 50) (elites 2) (draw-mode 'best))
  "Runs the algorithm. The world is created with CIRCLES number of
circles and a population size of POP-SIZE. The number of elites per
generation is ELITES. DRAW-MODE determines what circles will be
drawn. BEST mode only draws the best two circles. DRAW-VIABLE only
draws circles with non-zero fitness. DRAW-ALL draws all circles in the
population."
  (let* ((w (create-world circles))
	 (p (create-initial-population w :n pop-size :elites elites))
	 (iter 0))
    (sdl:with-init ()
      (sdl:window (world-max-x w) (world-max-y w)
		  :title-caption "GA Circles"
		  :icon-caption "GA Circles")
      (setf (sdl:frame-rate) 0)
      (sdl:update-display)
      (sdl:with-events ()
	(:idle
	 (setf p (next-generation w p))
	 (incf iter)
	 (sdl:clear-display sdl:*black*)
	 ; Draw all the circles in the world.
	 (dolist (c (world-circles w))
	   (draw-outlined-circle c +world-outline-color+ +world-color+))
	 ; Draw the population - either all circles or just the best two.
	 (draw-outlined-circle 
	  (decode-chromosome (first (population-members p))) 
	  +best-outline-color+ +best-color+)
	 (let ((other-circles
		(case draw-mode 
		  (:draw-all (rest (population-members p)))
		  (:draw-viable (rest (find-viable w p)))
		  (otherwise '()))))
	   (dolist (c (mapcar #'decode-chromosome other-circles))
	     (draw-circle c +default-color+)))
	 (format t "Generation ~a: ~a ~a~%" iter (population-best-fitness p) 
		 (population-total-fitness p))
	 (sdl:update-display))
	(:quit-event () t)
	(:key-down-event ()
			 (when (sdl:key-down-p :sdl-key-escape)
			   (sdl:push-quit-event)))
	(:video-expose-event () (sdl:update-display))))))
    