;;;; +----------------------------------------------------------------+
;;;; | zonquerer                                                      |
;;;; +----------------------------------------------------------------+
;;;; | Copyright (C) 2021 death                                       |
;;;; |                                                                |
;;;; | This program is free software: you can redistribute it and/or  |
;;;; | modify it under the terms of the GNU Affero General Public     |
;;;; | License as published by the Free Software Foundation, either   |
;;;; | version 3 of the License, or (at your option) any later        |
;;;; | version.                                                       |
;;;; |                                                                |
;;;; | This program is distributed in the hope that it will be        |
;;;; | useful, but WITHOUT ANY WARRANTY; without even the implied     |
;;;; | warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR        |
;;;; | PURPOSE.  See the GNU Affero General Public License for more   |
;;;; | details.                                                       |
;;;; |                                                                |
;;;; | You should have received a copy of the GNU Affero General      |
;;;; | Public License along with this program.  If not, see           |
;;;; | <https://www.gnu.org/licenses/>.                               |
;;;; +----------------------------------------------------------------+

(defpackage #:zonquerer/game
  (:use
   #:cl
   #:zonquerer/protocols
   #:zonquerer/resources
   #:zonquerer/utils)
  (:import-from
   #:sdl2)
  (:import-from
   #:sdl2-gfx)
  (:import-from
   #:autowrap)
  (:export
   #:standard-game
   #:driver))

(in-package #:zonquerer/game)

(defclass standard-game (game)
  ((video-dimensions :initform #C(320 200) :reader video-dimensions)
   (window :initform nil :accessor window)
   (renderer :initform nil :accessor renderer)
   (keys :initform nil :accessor keys)
   (mouse-position :initform #C(0 0) :accessor mouse-position)
   (ticks :initform 0 :accessor ticks)
   (current-cursor :initform nil :accessor current-cursor)
   (next-cursor :initform nil :accessor next-cursor)
   (assets-directory :initform #p"/home/death/lisp/zonquerer/assets/" :reader assets-directory)
   (resources :initform (make-hash-table :test 'equal) :reader resources)))

(defmethod find-resource ((game standard-game) kind name)
  (gethash (list kind name) (resources game)))

(defmethod (setf find-resource) (resource (game standard-game) kind name)
  (setf (gethash (list kind name) (resources game)) resource))

(defmethod free-all-resources ((game standard-game))
  (let* ((resources (resources game))
         (list (loop for resource being each hash-value of resources
                     collect resource)))
    (dolist (resource list)
      (free resource))
    (assert (zerop (hash-table-count resources)))))

(defmethod remove-resource ((game standard-game) kind name)
  (remhash (list kind name) (resources game)))

(defmethod update :after ((game standard-game) dt)
  (declare (ignore dt))
  (when (member :scancode-escape (keys game))
    (sdl2:push-quit-event)))

(defmethod draw :around ((game standard-game) dt)
  (declare (ignore dt))
  (let ((current-cursor (current-cursor game))
        (next-cursor (next-cursor game)))
    (when (not (eq current-cursor next-cursor))
      (set-cursor game next-cursor)))
  (let ((renderer (renderer game)))
    (sdl2:render-clear renderer)
    (call-next-method)
    (sdl2:render-present renderer)))

(defun mouse-event (game state button position)
  (declare (ignore game))
  (format t "Mouse event ~S~%" (list state button position)))

(defun window-event (game id)
  (declare (ignore game))
  (format t "Window event ~S~%" id))

(defmethod request-cursor ((game standard-game) name)
  (setf (next-cursor game) name))

(defun set-cursor (game name)
  (sdl2-ffi.functions:sdl-set-cursor
   (external (intern-resource game 'cursor name)))
  (setf (current-cursor game) name))

(defmethod event-loop ((game standard-game))
  (setf (ticks game) (sdl2:get-ticks))
  (sdl2:with-event-loop (:recursive t)
    (:quit () t)
    (:keydown
     (:keysym keysym)
     (let ((scancode (sdl2:scancode keysym)))
       (pushnew scancode (keys game))))
    (:keyup
     (:keysym keysym)
     (let ((scancode (sdl2:scancode keysym)))
       (setf (keys game)
             (delete scancode (keys game) :count 1))))
    (:mousemotion
     (:x x :y y)
     (setf (mouse-position game) (point x y)))
    (:mousebuttondown
     (:button button :x x :y y)
     (mouse-event game :down button (point x y)))
    (:mousebuttonup
     (:button button :x x :y y)
     (mouse-event game :up button (point x y)))
    (:idle
     ()
     (let* ((ticks (sdl2:get-ticks))
            (dt (* (- ticks (ticks game)) 1e-3)))
       (setf (ticks game) ticks)
       (update game dt)
       (draw game dt)))
    (:windowevent
     (:event event)
     (let ((id (autowrap:enum-key 'sdl2-ffi:sdl-window-event-id event)))
       (window-event game id)))))

(defun driver (game)
  (sdl2:with-init (:video)
    (destructure-point (width height) (video-dimensions game)
      (sdl2:with-window (window :w 0 :h 0 :flags '(:shown :fullscreen-desktop))
        (sdl2:with-renderer (renderer window :flags '(:accelerated))
          (sdl2-ffi.functions:sdl-set-hint sdl2-ffi:+sdl-hint-render-scale-quality+ "nearest")
          (sdl2-ffi.functions:sdl-render-set-logical-size renderer width height)
          (sdl2-ffi.functions:sdl-set-window-grab window :true)
          (sdl2:set-render-draw-color renderer 0 0 0 255)
          (setf (window game) window)
          (setf (renderer game) renderer)
          (unwind-protect
               (event-loop game)
            (free-all-resources game)))))))
