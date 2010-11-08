(in-package #:ga-circles-gui)

(defun gui-test ()
  (display-world (create-world)))
  
(defun display-world (world &optional (population nil))
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
    (let ((color (sdl:color :r 0 :g 255 :b 0 :a 255)))
      (when (not (null population))
	(dolist (c (reverse (mapcar #'decode-chromosome 
				    (find-viable world population))))
	  (sdl:draw-filled-circle-* (circle-x c) (circle-y c) (circle-radius c)
				    :color color :alpha 255
				    :surface sdl:*default-display*)
	  (setf color (sdl:color :r (random 255) :g (random 255) :b (random 255) :a 32)))))
    (sdl:update-display)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event ()
		       (when (sdl:key-down-p :sdl-key-escape)
			 (sdl:push-quit-event)))
      (:video-expose-event () (sdl:update-display)))))
