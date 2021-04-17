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

(defpackage #:zonquerer/zonquerer
  (:use
   #:cl
   #:zonquerer/protocols
   #:zonquerer/resources
   #:zonquerer/utils
   #:zonquerer/game)
  (:export
   #:play))

(in-package #:zonquerer/zonquerer)

;;;; Cursor management

(defclass cursors-mixin ()
  ())

(defvar *cursor-hotspots*
  '((:cursor-select 16 16)
    (:cursor-left 0 16)
    (:cursor-right 31 16)
    (:cursor-up 16 0)
    (:cursor-down 16 31)))

(defmethod event-loop :before ((game cursors-mixin))
  (loop for (cursor-name hx hy) in *cursor-hotspots*
        do (intern-resource game 'cursor cursor-name :hot (point hx hy)))
  (request-cursor game :cursor-select))

;;;; Tile map rendering & scrolling

(defclass tile-map-mixin (cursors-mixin)
  ((map-start-position :initform #C(0 0) :accessor map-start-position)
   (tile-map-renderer :initform nil :accessor tile-map-renderer)
   (tile-map-scroller :initform nil :accessor tile-map-scroller)))

(defun make-tile-map-scroller (tile-map)
  (let ((game (game tile-map))
        (margin 1))
    (destructure-point (vw vh) (video-dimensions game)
      (destructure-point (tmw tmh) (dimensions tile-map)
        (lambda (dt)
          (destructure-point (mx my) (mouse-position game)
            (let ((pos (map-start-position game))
                  (c :cursor-select))
              (when (< mx margin)
                (setf c :cursor-left)
                (when (plusp (x pos))
                  (setf pos (- pos (* dt #C(200 0))))))
              (when (> mx (- vw margin 1))
                (setf c :cursor-right)
                (when (< (x pos) (- tmw vw))
                  (setf pos (+ pos (* dt #C(200 0))))))
              (when (< my margin)
                (setf c :cursor-up)
                (when (plusp (y pos))
                  (setf pos (- pos (* dt #C(0 200))))))
              (when (> my (- vh margin 1))
                (setf c :cursor-down)
                (when (< (y pos) (- tmh vh))
                  (setf pos (+ pos (* dt #C(0 200))))))
              (request-cursor game c)
              (setf (map-start-position game) pos))))))))

(defmethod update :before ((game tile-map-mixin) dt)
  (funcall (tile-map-scroller game) dt))

(defmethod draw :before ((game tile-map-mixin) dt)
  (declare (ignore dt))
  (funcall (tile-map-renderer game) (map-start-position game)))

(defmethod event-loop :before ((game tile-map-mixin))
  (let ((tile-map (intern-resource game 'tile-map :map-01)))
    (setf (tile-map-renderer game) (external tile-map))
    (setf (tile-map-scroller game) (make-tile-map-scroller tile-map))))

;;;; Zonquerer game

(defclass zonquerer (tile-map-mixin standard-game)
  ((anim :initform nil :accessor anim)))

(defmethod update ((game zonquerer) dt)
  (declare (ignore dt)))

(defmethod draw ((game zonquerer) dt)
  (let ((dest-rect (sdl2:make-rect 0 150 320 50)))
    (sdl2:render-copy (renderer game)
                      (external (intern-resource game 'texture :panel))
                      :dest-rect dest-rect)
    (sdl2:free-rect dest-rect))
  (funcall (anim game) (mouse-position game) dt))

(defmethod event-loop :before ((game zonquerer))
  (setf (anim game) (funcall (external (intern-resource game 'sprite-sheet :unit)) :down)))

(defun play ()
  (driver (make-instance 'zonquerer)))
