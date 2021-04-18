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
   (map-renderer :initform nil :accessor map-renderer)
   (map-scroller :initform nil :accessor map-scroller)
   (panel-height :initform 50 :reader panel-height)
   (units :initform '() :accessor units)))

;;;; Cursors

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

;;;; Panel

(defun draw-panel (game dt)
  (declare (ignore dt))
  (let ((dest-rect (sdl2:make-rect 0 150 320 50)))
    (sdl2:render-copy (renderer game)
                      (external (intern-resource game 'texture :panel))
                      :dest-rect dest-rect)
    (sdl2:free-rect dest-rect)))

;;;; Map

(defun make-map-scroller (tile-map)
  (let ((game (game tile-map))
        (margin 1))
    (destructure-point (vw vh) (video-dimensions game)
      (destructure-point (tmw tmh) (dimensions tile-map)
        ;; Account for panel.
        (incf tmh (panel-height game))
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

(defun update-map (game dt)
  (funcall (map-scroller game) dt))

(defun draw-map (game dt)
  (declare (ignore dt))
  (funcall (map-renderer game) (map-start-position game)))

(defun setup-map (game)
  (let ((tile-map (intern-resource game 'tile-map :map-01)))
    (setf (map-renderer game) (external tile-map))
    (setf (map-scroller game) (make-map-scroller tile-map))))

;;;; Units

(defclass unit ()
  ((game :initarg :game :reader game)
   (position-on-map :initarg :position-on-map :reader position-on-map)
   (heading :initarg :heading :reader heading)
   (state :initarg :state :reader state)
   (anim :initform nil :accessor anim)))

(defun position-on-screen (unit)
  (- (position-on-map unit)
     (map-start-position (game unit))))

(defmethod initialize-instance :after ((unit unit) &key)
  (let* ((game (game unit))
         (sprite-sheet (intern-resource game 'sprite-sheet :unit))
         (animator (external sprite-sheet))
         (state (state unit))
         (heading (heading unit))
         (tag (list state heading)))
    (setf (anim unit) (funcall animator tag))))

(defun update-unit (unit dt)
  (declare (ignore unit dt)))

(defun draw-unit (unit dt)
  (funcall (anim unit)
           (position-on-screen unit)
           dt))

(defun setup-units (game)
  (dotimes (i 4)
    (let ((unit (make-instance 'unit
                               :game game
                               :position-on-map (point (+ (* i 50) 50)
                                                       (if (evenp i) 40 20))
                               :state (if (= i 1)
                                          :walk
                                          :idle)
                               :heading :down)))
      (push unit (units game)))))

;;;; The game

(defmethod update ((game zonquerer) dt)
  (update-map game dt)
  (dolist (unit (units game))
    (update-unit unit dt)))

(defmethod draw ((game zonquerer) dt)
  (draw-map game dt)
  (dolist (unit (units game))
    (draw-unit unit dt))
  (draw-panel game dt))

(defmethod event-loop :before ((game zonquerer))
  (setup-cursors game)
  (setup-map game)
  (setup-units game))

(defun play ()
  (driver (make-instance 'zonquerer)))
