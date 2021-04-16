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
  (:import-from
   #:plus-c)
  (:export
   #:play))

(in-package #:zonquerer/game)

(defclass zonquerer (game)
  ((video-dimensions :initform #C(320 200) :reader game-video-dimensions)
   (frame-rate :initform 30 :reader game-frame-rate)
   (window :initform nil :accessor game-window)
   (renderer :initform nil :accessor game-renderer)
   (fps-manager :initform nil :accessor game-fps-manager)
   (keys :initform nil :accessor game-keys)
   (mouse-position :initform #C(0 0) :accessor game-mouse-position)
   (ticks :initform 0 :accessor game-ticks)
   (current-cursor :initform nil :accessor game-current-cursor)
   (next-cursor :initform nil :accessor game-next-cursor)
   (assets-directory :initform #p"/home/death/lisp/zonquerer/assets/" :reader assets-directory)
   (resources :initform (make-hash-table :test 'equal) :reader resources)))

(defmethod find-resource ((game zonquerer) kind name)
  (gethash (list kind name) (resources game)))

(defmethod (setf find-resource) (resource (game zonquerer) kind name)
  (setf (gethash (list kind name) (resources game)) resource))

(defmethod free-all-resources ((game zonquerer))
  (let* ((resources (resources game))
         (list (loop for resource being each hash-value of resources
                     collect resource)))
    (dolist (resource list)
      (free resource))
    (assert (zerop (hash-table-count resources)))))

(defmethod remove-resource ((game zonquerer) kind name)
  (remhash (list kind name) (resources game)))

(defun update-map-scrolling (game)
  (destructure-point (mx my) (game-mouse-position game)
    (destructure-point (vw vh) (game-video-dimensions game)
      (let ((margin 1)
            (c :cursor-select))
        (cond ((< mx margin)
               (setf c :cursor-left))
              ((> mx (- vw margin 1))
               (setf c :cursor-right))
              ((< my margin)
               (setf c :cursor-up))
              ((> my (- vh margin 1))
           (setf c :cursor-down)))
        (setf (game-next-cursor game) c)))))

(defmethod update ((game zonquerer))
  (setf (game-ticks game) (sdl2:get-ticks))
  (update-map-scrolling game)
  (when (member :scancode-escape (game-keys game))
    (sdl2:push-quit-event)))

(defmethod draw ((game zonquerer))
  (let ((renderer (game-renderer game))
        (current-cursor (game-current-cursor game))
        (next-cursor (game-next-cursor game)))
    (when (not (eq current-cursor next-cursor))
      (set-cursor game next-cursor))
    (sdl2:render-clear renderer)
    (sdl2:render-present renderer)))

(defun mouse-event (game state button position)
  (declare (ignore game))
  (format t "Mouse event ~S~%" (list state button position)))

(defun window-event (game id)
  (declare (ignore game))
  (format t "Window event ~S~%" id))

(defun set-cursor (game name)
  (sdl2-ffi.functions:sdl-set-cursor
   (external (intern-resource game 'cursor name)))
  (setf (game-current-cursor game) name))

(defmacro with-game-resources ((game) &body body)
  `(unwind-protect
        (progn ,@body)
     (free-all-resources ,game)))

(defvar *cursor-hotspots*
  '((:cursor-select 16 16)
    (:cursor-left 0 16)
    (:cursor-right 31 16)
    (:cursor-up 16 0)
    (:cursor-down 16 31)))

(defun play ()
  (let ((game (make-instance 'zonquerer)))
    (sdl2:with-init (:video)
      (destructure-point (width height) (game-video-dimensions game)
        (sdl2:with-window (window :w 0 :h 0 :flags '(:shown :fullscreen-desktop))
          (sdl2:with-renderer (renderer window)
            (sdl2-ffi.functions:sdl-set-hint sdl2-ffi:+sdl-hint-render-scale-quality+ "linear")
            (sdl2-ffi.functions:sdl-render-set-logical-size renderer width height)
            (plus-c:c-let ((fps-manager sdl2-ffi:fp-smanager :free t))
              (sdl2-ffi.functions:sdl-init-framerate (autowrap:ptr fps-manager))
              (sdl2-ffi.functions:sdl-set-framerate (autowrap:ptr fps-manager)
                                                    (game-frame-rate game))
              (sdl2-ffi.functions:sdl-set-window-grab window :true)
              (sdl2:set-render-draw-color renderer 0 0 0 255)
              (setf (game-window game) window)
              (setf (game-renderer game) renderer)
              (setf (game-fps-manager game) fps-manager)
              (with-game-resources (game)
                (loop for (cursor-name hx hy) in *cursor-hotspots*
                      do (intern-resource game 'cursor cursor-name :hot (point hx hy)))
                (set-cursor game :cursor-select)
                (sdl2:with-event-loop (:recursive t)
                  (:quit () t)
                  (:keydown
                   (:keysym keysym)
                   (let ((scancode (sdl2:scancode keysym)))
                     (pushnew scancode (game-keys game))
                     (update game)))
                  (:keyup
                   (:keysym keysym)
                   (let ((scancode (sdl2:scancode keysym)))
                     (setf (game-keys game)
                           (delete scancode (game-keys game) :count 1))
                     (update game)))
                  (:mousemotion
                   (:x x :y y)
                   (setf (game-mouse-position game) (point x y))
                   (update game))
                  (:mousebuttondown
                   (:button button :x x :y y)
                   (mouse-event game :down button (point x y))
                   (update game))
                  (:mousebuttonup
                   (:button button :x x :y y)
                   (mouse-event game :up button (point x y))
                   (update game))
                  (:idle
                   ()
                   (update game)
                   (draw game)
                   (sdl2-ffi.functions:sdl-framerate-delay (autowrap:ptr fps-manager)))
                  (:windowevent
                   (:event event)
                   (let ((id (autowrap:enum-key 'sdl2-ffi:sdl-window-event-id event)))
                     (window-event game id)
                     (update game))))))))))))
