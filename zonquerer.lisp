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

(defclass zonquerer (standard-game)
  ((map-start-position :initform #C(0 0) :accessor map-start-position)
   (tile-map :initform nil :accessor tile-map)
   (anim :initform nil :accessor anim)))

(defun update-map-scrolling (game dt)
  (destructure-point (mx my) (mouse-position game)
    (destructure-point (vw vh) (video-dimensions game)
      (destructure-point (tmw tmh) (dimensions (tile-map game))
        (let ((margin 1)
              (c :cursor-select)
              (pos (map-start-position game)))
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
          (setf (map-start-position game) pos))))))

(defmethod update ((game zonquerer) dt)
  (update-map-scrolling game dt))

(defmethod draw ((game zonquerer) dt)
  (render-map (tile-map game) (map-start-position game))
  (funcall (anim game) (mouse-position game) dt))

(defvar *cursor-hotspots*
  '((:cursor-select 16 16)
    (:cursor-left 0 16)
    (:cursor-right 31 16)
    (:cursor-up 16 0)
    (:cursor-down 16 31)))

(defun setup-cursors (game)
  (loop for (cursor-name hx hy) in *cursor-hotspots*
        do (intern-resource game 'cursor cursor-name :hot (point hx hy)))
  (request-cursor game :cursor-select))

(defmethod event-loop :before ((game zonquerer))
  (setup-cursors game)
  (setf (tile-map game) (intern-resource game 'tile-map :map-01))
  (setf (anim game) (sprite-animator (intern-resource game 'sprite-sheet :unit) :down)))

(defun play ()
  (driver (make-instance 'zonquerer)))
