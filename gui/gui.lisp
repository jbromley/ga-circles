(defpackage #:ga-circles-gui
  (:use #:cl #:ga-circles)
  (:export
   #:display-world))

(in-package #:ga-circles-gui)

(defun display-world (world &rest circles)
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
    (let ((color (sdl:color :r 0 :g 192 :b 0 :a 128)))
      (dolist (c circles)
	(sdl:draw-filled-circle-* (circle-x c) (circle-y c) (circle-radius c)
				  :color color :alpha 255
				  :surface sdl:*default-display*)
	(setf color (sdl:color :r 64 :g 64 :b 64 :a 128))))
	
    (sdl:update-display)
    (sdl:with-events ()
      (:quit-event () t)
      (:video-expose-event () (sdl:update-display)))))
