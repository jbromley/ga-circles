(in-package #:ga-circles-gui)

(defun gui-test ()
  (display-world (create-world)))
  
(defun draw (world-circles pop-circles)
    (dolist (c world-circles)
      (sdl:draw-filled-circle-* (circle-x c) (circle-y c) (circle-radius c)
				:color (sdl:color :r 0 :g 0 :b 192 :a 128) 
				:alpha 255
				:surface sdl:*default-display*))
    (dolist (c pop-circles)
      (sdl:draw-filled-circle-* (circle-x c) (circle-y c) (circle-radius c)
				:color (sdl:color :r 0 :g 255 :b 0 :a 192)
				:alpha 255
				:surface sdl:*default-display*)))
  
(defun display-world (world &optional (population nil))
  "Display the environment WORLD with the population in CIRCLES."
  (sdl:with-init ()
    (sdl:window (world-max-x world) (world-max-y world)
		:title-caption "GA Circles"
		:icon-caption "GA Circles")
    (setf (sdl:frame-rate) 0)
    (draw (world-circles world) 
	  (reverse (mapcar #'decode-chromosome (find-viable world population))))
    (sdl:update-display)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event ()
		       (when (sdl:key-down-p :sdl-key-escape)
			 (sdl:push-quit-event)))
      (:video-expose-event () (sdl:update-display)))))

(defun run (&key (circles 50) (pop-size 50) (elites 4) (draw-all nil))
  (let* ((w (create-world circles))
	 (p (create-initial-population w :n pop-size :elites elites))
	 (iter 0))
    (sdl:with-init ()
      (sdl:window (world-max-x w) (world-max-y w)
		  :title-caption "GA Circles"
		  :icon-caption "GA Circles")
      (setf (sdl:frame-rate) 0)
      (draw (world-circles w) (population-most-fit-circles p))
      (sdl:update-display)
      (sdl:with-events ()
	(:idle
	 (setf p (next-generation w p))
	 (incf iter)
	 (sdl:clear-display sdl:*black*)
	 (if draw-all
	     (draw (world-circles w) 
		   (mapcar #'decode-chromosome (find-viable w p)))
	     (draw (world-circles w) (population-most-fit-circles p 2)))
	 (format t "Generation ~a: ~a ~a~%" iter (population-best-fitness p) 
		 (population-total-fitness p))
	 (sdl:update-display))
	(:quit-event () t)
	(:key-down-event ()
			 (when (sdl:key-down-p :sdl-key-escape)
			   (sdl:push-quit-event)))
	(:video-expose-event () (sdl:update-display))))))
    