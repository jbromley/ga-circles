(in-package #:ga-circles-gui)

(defun test ()
  (let ((world (make-world)))
    (populate-world world)
    (display-world world)))
  
(defun display-world (world &optional (circles '()))
  "Display the environment WORLD with the population in CIRCLES."
  (sdl:with-init ()
    (sdl:window (world-max-x world) (world-max-y world)
		:title-caption "GA Circles"
		:icon-caption "GA Circles")
    (setf (sdl:frame-rate) 0)
    (dolist (c (world-circles world))
      (sdl:draw-filled-circle-* (circle-x c) (circle-y c) (circle-radius c)
				:color (sdl:color :r 0 :g 0 :b 192 :a 128) 
				:alpha 255
				:surface sdl:*default-display*))
    (let ((color (sdl:color :r 0 :g 192 :b 0 :a 128)))
      (dolist (c circles)
	(sdl:draw-filled-circle-* (circle-x c) (circle-y c) (circle-radius c)
				  :color color :alpha 255
				  :surface sdl:*default-display*)
	(setf color (sdl:color :r 64 :g 64 :b 64 :a 128))))
    (sdl:update-display)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event ()
		       (when (sdl:key-down-p :sdl-key-escape)
			 (sdl:push-quit-event)))
      (:video-expose-event () (sdl:update-display)))))
