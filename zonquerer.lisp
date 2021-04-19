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
   #:zonquerer/game
   #:zonquerer/a-star)
  (:export
   #:play))

(in-package #:zonquerer/zonquerer)

(defclass zonquerer (standard-game)
  ((map-start-position-float :initform #C(0.0 0.0) :accessor map-start-position-float)
   (map-dimensions :initform nil :accessor map-dimensions)
   (map-renderer :initform nil :accessor map-renderer)
   (map-scroller :initform nil :accessor map-scroller)
   (panel-height :initform 50 :reader panel-height)
   (units :initform '() :accessor units)
   (selection-event-buildup :initform nil :accessor selection-event-buildup)
   (string-renderer :initform nil :accessor string-renderer)
   (occupancy-table :initform nil :accessor occupancy-table)
   (debugging :initform nil :accessor debuggingp)))

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

;;;; Font

(defun setup-font (game)
  (let ((font (intern-resource game
                               'font
                               :tinyfont
                               :alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+-!?%():;/\\.^*<>[]{}='\"`,~abcdefghi"
                               :glyph-dimensions #C(3 5))))
    (setf (string-renderer game) (external font))))

;;;; Map

(defun map-start-position (game)
  (truncate-point (map-start-position-float game)))

(defun make-map-scroller (tile-map)
  (let ((game (game tile-map))
        (margin 1))
    (destructure-point (vw vh) (video-dimensions game)
      (destructure-point (tmw tmh) (dimensions tile-map)
        ;; Account for panel.
        (incf tmh (panel-height game))
        (lambda (dt)
          (destructure-point (mx my) (mouse-position game)
            (let ((pos (map-start-position-float game))
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
              (setf (map-start-position-float game) pos))))))))

(defun update-map (game dt)
  (funcall (map-scroller game) dt))

(defun draw-map (game dt)
  (declare (ignore dt))
  (funcall (map-renderer game) (map-start-position game)))

(defun setup-map (game)
  (let ((tile-map (intern-resource game 'tile-map :map-01)))
    (setf (map-dimensions game) (dimensions tile-map))
    (setf (map-renderer game) (external tile-map))
    (setf (map-scroller game) (make-map-scroller tile-map))))

;;;; Occupancy Table

(defconstant occupancy-dim 16)

(defconstant occupancy-dim-1 (1- occupancy-dim))

(defun map-position-to-cell (position)
  (truncate-point position occupancy-dim))

(defun cell-to-map-position (cell)
  (* cell occupancy-dim))

(defun screen-position-to-cell (game position)
  (map-position-to-cell (- position (map-start-position game))))

(defun cell-to-screen-position (game cell)
  (- (cell-to-map-position cell) (map-start-position game)))

(defun occupy (game-or-table cell object)
  (let ((table (if (typep game-or-table 'game)
                   (occupancy-table game-or-table)
                   game-or-table)))
    (if (null object)
        (remhash cell table)
        (setf (gethash cell table) object))))

(defun occupiedp (game cell)
  (let ((table (occupancy-table game)))
    (gethash cell table)))

(defconstant cell-max-near-distance 5)

(defun find-nearest-unoccupied-cell (game cell)
  (loop for scale from 1 to cell-max-near-distance
        do (loop for direction in *all-directions-including-diagonals*
                 for near-cell = (+ cell (* scale direction))
                 when (not (occupiedp game near-cell))
                 do (return-from find-nearest-unoccupied-cell near-cell)))
  (warn "Couldn't find a cell near ~S that is unoccupied." cell)
  nil)

(defun draw-occupancy-table (game dt)
  (declare (ignore dt))
  (let* ((pos (map-start-position game))
         (dims (video-dimensions game))
         (start-cell (map-position-to-cell pos))
         (end-cell (map-position-to-cell (+ pos dims)))
         (renderer (renderer game)))
    (destructure-point (sx sy) start-cell
      (destructure-point (ex ey) end-cell
        (do ((y sy (1+ y)))
            ((> y ey))
          (do ((x sx (1+ x)))
              ((> x ex))
            (let ((cell (point x y)))
              (when (occupiedp game cell)
                (destructure-point (px py) (cell-to-screen-position game cell)
                  (sdl2-ffi.functions:box-rgba renderer
                                               px
                                               py
                                               (+ px occupancy-dim-1)
                                               (+ py occupancy-dim-1)
                                               #xFF #x00 #x00 #x1F))))))))))

(defun make-occupancy-table (map-dimensions)
  (let ((table (make-hash-table)))
    (destructure-point (mw mh) (map-position-to-cell map-dimensions)
      (occupy table (point -1 -1) t)
      (dotimes (x (1+ mw))
        (occupy table (point x -1) t)
        (occupy table (point x mh) t))
      (dotimes (y mh)
        (occupy table (point -1 y) t)
        (occupy table (point mw y) t)))
    table))

(defun setup-occupancy-table (game)
  (setf (occupancy-table game)
        (make-occupancy-table (map-dimensions game))))

;;;; Units

(defconstant unit-dim 16)

(defconstant unit-dim-1 (1- unit-dim))

(defconstant unit-center
  (point (truncate unit-dim 2)
         (truncate unit-dim 2)))

(defconstant walk-speed-min 20)

(defconstant walk-speed-max 50)

(defclass unit ()
  ((game :initarg :game :reader game)
   (position-on-map-float :initarg :position-on-map :accessor position-on-map-float)
   (heading :initarg :heading :reader heading)
   (state :initform nil :accessor state)
   (selected :initform nil :accessor selectedp)
   (animator :initform nil :accessor animator)
   (anim :initform nil :accessor anim)
   (action :initform nil :accessor action)
   (path-to-walk :initform '() :accessor path-to-walk)
   (path-to-walk-blocked-cells :initform nil :accessor path-to-walk-blocked-cells)
   (walk-speed :initform walk-speed-min :accessor walk-speed)))

(defun position-on-map (unit)
  (truncate-point (position-on-map-float unit)))

(defun position-on-screen (unit)
  (- (position-on-map unit)
     (map-start-position (game unit))))

(defmethod initialize-instance :after ((unit unit) &key)
  (let* ((game (game unit))
         (sprite-sheet (intern-resource game 'sprite-sheet :unit))
         (animator (external sprite-sheet)))
    (setf (animator unit) animator)
    (setf (state unit) :idle)
    (unit-occupy unit)))

(defun unit-occupancy-cells (map-position)
  (list (map-position-to-cell map-position)
        (map-position-to-cell (+ map-position (point 0 unit-dim-1)))
        (map-position-to-cell (+ map-position (point unit-dim-1 0)))
        (map-position-to-cell (+ map-position (point unit-dim-1 unit-dim-1)))))

(defun unit-can-occupy-p (unit map-position)
  (let ((game (game unit)))
    (every (lambda (cell)
             (let ((occupant (occupiedp game cell)))
               (or (null occupant)
                   (eq occupant unit))))
           (unit-occupancy-cells map-position))))

(defun unit-occupy (unit &optional (object unit))
  (let ((pos (position-on-map unit)))
    (if (unit-can-occupy-p unit pos)
        (let ((game (game unit))
              (cells (unit-occupancy-cells pos)))
          (dolist (cell cells)
            (occupy game cell object)))
        (error "Unit ~S can't occupy position ~S with object ~S." unit pos object))))

(defmethod (setf state) :around (new-state (unit unit))
  (let ((old-state (state unit)))
    (prog1 (call-next-method)
      (unless (eq old-state new-state)
        (let* ((heading (heading unit))
               (tag (list new-state heading)))
          (setf (anim unit) (funcall (animator unit) tag)))))))

(defun move-to-unoccupied-adjacent-cell (unit)
  (let ((cell (map-position-to-cell (position-on-map unit))))
    (loop for scale from 1 to 2
          do (loop for direction in *taxicab-directions*
                   for adjacent-cell = (+ cell (* scale direction))
                   for adjacent-cell-position = (cell-to-map-position adjacent-cell)
                   when (unit-can-occupy-p unit adjacent-cell-position)
                   do (setf (path-to-walk unit) (list adjacent-cell-position))
                      (return-from move-to-unoccupied-adjacent-cell))))
  ;; The unit is stuck in a bad place.  Time to die.
  (setf (state unit) :die))

(defun unit-do-move (unit)
  (let* ((goal-position (- (cadr (action unit)) unit-center))
         (goal-position-cell (map-position-to-cell goal-position))
         (game (game unit)))
    (setf (path-to-walk-blocked-cells unit) (make-hash-table))
    ;; It's not guaranteed that we currently occupy our position, as
    ;; we can move to a different goal position while already walking,
    ;; so we need to test for it.
    (when (unit-can-occupy-p unit (position-on-map unit))
      (unit-occupy unit nil))
    (when (occupiedp game goal-position-cell)
      (let ((near (find-nearest-unoccupied-cell game goal-position-cell)))
        (when (null near)
          (unit-occupy unit)
          (return-from unit-do-move))
        (setf goal-position (+ (cell-to-map-position near) unit-center))))
    (setf (path-to-walk unit)
          (compute-path-to-walk (position-on-map unit)
                                goal-position
                                (list (occupancy-table game)
                                      (path-to-walk-blocked-cells unit))))
    (setf (state unit) :walk)))

(defun update-unit (unit dt)
  (let ((game (game unit)))
    (when (eq (car (action unit)) :move)
      (unit-do-move unit)
      (setf (action unit) nil))
    (case (state unit)
      (:walk
       (let ((next-position (car (path-to-walk unit))))
         (cond ((null next-position)
                (cond ((unit-can-occupy-p unit (position-on-map unit))
                       (unit-occupy unit)
                       (setf (state unit) :idle))
                      (t
                       (move-to-unoccupied-adjacent-cell unit))))
               ((= next-position (position-on-map unit))
                (pop (path-to-walk unit)))
               ((unit-can-occupy-p unit next-position)
                (setf (state unit) :walk)
                (incf (position-on-map-float unit)
                      (* dt
                         (walk-speed unit)
                         (signum-point (- next-position (position-on-map unit))))))
               (t
                ;; Mark next position's cell as blocked and search for a
                ;; new path.
                (let* ((cell (map-position-to-cell next-position))
                       (target-position (car (last (path-to-walk unit)))))
                  (setf (gethash cell (path-to-walk-blocked-cells unit)) t)
                  (setf (path-to-walk unit)
                        (compute-path-to-walk (position-on-map unit)
                                              target-position
                                              (list (occupancy-table game)
                                                    (path-to-walk-blocked-cells unit))))
                  (when (null (path-to-walk unit))
                    (cond ((unit-can-occupy-p unit (position-on-map unit))
                           (unit-occupy unit)
                           (setf (state unit) :idle))
                          (t
                           ;; There is no path to the target position,
                           ;; and we can't occupy the current
                           ;; position, so find an adjacent cell to
                           ;; move to.
                           (move-to-unoccupied-adjacent-cell unit)))))))))
      (:die
       (format t "TODO: Unit ~S is supposed to die.~%" unit)
       (setf (state unit) :idle)))))

(defun compute-path-to-walk (start-position goal-position occupancy-tables)
  (let ((start-cell (map-position-to-cell start-position))
        (goal-cell (map-position-to-cell goal-position)))
    (flet ((blockedp (cell)
             (some (lambda (table) (gethash cell table)) occupancy-tables)))
      (if (or (blockedp start-cell) (blockedp goal-cell))
          '()
          (reverse
           (a-star start-position
                   :goal-state-p (lambda (position)
                                   (= position goal-position))
                   :heuristic (lambda (position)
                                (abs (- position goal-position)))
                   :failure-p (lambda (position)
                                (blockedp (map-position-to-cell position)))
                   :max-cost (* 5 (abs (- goal-position start-position)))
                   :expand (lambda (position)
                             (loop with scale = (if (< (abs (- goal-position position)) occupancy-dim)
                                                    1
                                                    occupancy-dim)
                                   for direction in *taxicab-directions*
                                   for new-position = (+ position (* scale direction))
                                   collect (list (abs (- new-position position))
                                                 new-position)))))))))

(defun draw-unit (unit dt)
  (let ((pos (position-on-screen unit))
        (game (game unit)))
    (funcall (anim unit) pos dt)
    (when (selectedp unit)
      (sdl2-ffi.functions:rectangle-rgba (renderer game)
                                         (1- (x pos))
                                         (1- (y pos))
                                         (+ 1 unit-dim (x pos))
                                         (+ 1 unit-dim (y pos))
                                         #xFF #xFF #xFF #x7F))
    (when (debuggingp game)
      (dolist (pos (path-to-walk unit))
        (let ((spos (- pos (map-start-position game))))
          (sdl2-ffi.functions:pixel-rgba (renderer game)
                                         (x spos)
                                         (y spos)
                                         #x00 #x00 #x00 #x7F))))))

(defun setup-units (game)
  (dotimes (i 4)
    (let ((unit (make-instance 'unit
                               :game game
                               :position-on-map (point (+ (* i 50) 50)
                                                       (if (evenp i) 40 20))
                               :heading :down)))
      (push unit (units game)))))

(defun selected-units (game)
  (remove-if-not #'selectedp (units game)))

;;;; Selection

(defclass selection-event (standard-event)
  ((start-position :initarg :start-position :accessor start-position)
   (end-position :initarg :end-position :accessor end-position)))

(defclass selection-union-event (selection-event)
  ())

(defclass selection-toggle-event (selection-event)
  ())

(defun selection-bounds (start-position end-position)
  (values (floor (min (x start-position) (x end-position)))
          (floor (min (y start-position) (y end-position)))
          (ceiling (max (x start-position) (x end-position)))
          (ceiling (max (y start-position) (y end-position)))))

(defun draw-selection (game dt)
  (declare (ignore dt))
  (let* ((renderer (renderer game))
         (start-position (- (start-position (selection-event-buildup game))
                            (map-start-position game)))
         (end-position (mouse-position game)))
    (multiple-value-call #'sdl2-ffi.functions:rectangle-rgba
      renderer
      (selection-bounds start-position end-position)
      #xFF #xFF #xFF #x7F)))

(defun units-in-selection (game selection-event)
  (multiple-value-bind (left top right bottom)
      (selection-bounds (start-position selection-event)
                        (end-position selection-event))
    (flet ((insidep (point)
             (and (<= left (+ (x point) unit-dim-1) (+ right unit-dim-1))
                  (<= top (+ (y point) unit-dim-1) (+ bottom unit-dim-1)))))
      (remove-if-not #'insidep
                     (units game)
                     :key #'position-on-map))))

(defmethod process-event ((game zonquerer) (event selection-event) dt)
  (declare (ignore dt))
  (dolist (unit (units game))
    (setf (selectedp unit) nil))
  (dolist (unit (units-in-selection game event))
    (setf (selectedp unit) t)))

(defmethod process-event ((game zonquerer) (event selection-union-event) dt)
  (declare (ignore dt))
  (dolist (unit (units-in-selection game event))
    (setf (selectedp unit) t)))

(defmethod process-event ((game zonquerer) (event selection-toggle-event) dt)
  (declare (ignore dt))
  (dolist (unit (units-in-selection game event))
    (setf (selectedp unit)
          (not (selectedp unit)))))

;;;; Unit Movement

(defclass move-event (standard-event)
  ((units :initarg :units :reader units)
   (destination :initarg :destination :reader destination)))

(defmethod process-event ((game zonquerer) (event move-event) dt)
  (declare (ignore dt))
  (let ((destination (destination event)))
    (dolist (unit (units event))
      (setf (action unit)
            (list :move destination)))))

;;;; The game

(defmethod update ((game zonquerer) dt)
  (when (member :scancode-d (keys game))
    (setf (debuggingp game)
          (not (member :scancode-lshift (keys game)))))
  (update-map game dt)
  (dolist (unit (units game))
    (update-unit unit dt)))

(defmethod draw ((game zonquerer) dt)
  (draw-map game dt)
  (when (debuggingp game)
    (draw-occupancy-table game dt))
  (dolist (unit (units game))
    (draw-unit unit dt))
  (when (selection-event-buildup game)
    (draw-selection game dt))
  (draw-panel game dt))

(defmethod game-loop :before ((game zonquerer))
  (setup-cursors game)
  (setup-map game)
  (setup-occupancy-table game)
  (setup-units game)
  (setup-font game))

(defmethod mouse-event ((game zonquerer) (state (eql :down)) (button (eql 1)) position)
  (let ((keys (keys game)))
    (setf (selection-event-buildup game)
          (make-instance (cond ((member :scancode-lshift keys)
                                'selection-union-event)
                               ((member :scancode-lctrl keys)
                                'selection-toggle-event)
                               (t
                                'selection-event))
                         :start-position (+ (map-start-position game) position)
                         :end-position nil))))

(defmethod mouse-event ((game zonquerer) (state (eql :up)) (button (eql 1)) position)
  (let ((selection-event (selection-event-buildup game)))
    (cond (selection-event
           (setf (end-position selection-event)
                 (+ (map-start-position game) position))
           (push-event game selection-event)
           (setf (selection-event-buildup game) nil)))))

(defmethod mouse-event ((game zonquerer) (state (eql :down)) (button (eql 3)) position)
  (let ((units (selected-units game)))
    (when units
      (let ((move-event
              (make-instance 'move-event
                             :units units
                             :destination (+ (map-start-position game) position))))
        (push-event game move-event)))))

(defun play ()
  (driver (make-instance 'zonquerer)))
