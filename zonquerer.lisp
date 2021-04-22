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
  (:import-from
   #:alexandria)
  (:export
   #:play))

(in-package #:zonquerer/zonquerer)

(defclass zonquerer (standard-game)
  ((map-start-position-float :initform #C(0.0 0.0) :accessor map-start-position-float)
   (map-renderer :initform nil :accessor map-renderer)
   (map-scroller :initform nil :accessor map-scroller)
   (map-br-cell :initform nil :accessor map-br-cell)
   (units :initform '() :accessor units)
   (selection-event-buildup :initform nil :accessor selection-event-buildup)
   (string-renderer :initform nil :accessor string-renderer)
   (occupancy-table :initform nil :accessor occupancy-table)
   (debugging :initform nil :accessor debuggingp)
   (stored-selections :initform (make-hash-table) :reader stored-selections)
   (time-elapsed :initform 0.0 :accessor time-elapsed)
   (timers :initform '() :accessor timers)
   (end-game-anim :initform nil :accessor end-game-anim)
   (end-game-strings :initform nil :accessor end-game-strings)
   (play-again :initform nil :accessor play-again-p)))

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
  (let* ((tile-map (intern-resource game 'tile-map :map-01))
         (dims (dimensions tile-map)))
    (setf (map-start-position-float game) (/ (- dims (video-dimensions game)) 2))
    (setf (map-renderer game) (external tile-map))
    (setf (map-scroller game) (make-map-scroller tile-map))
    (setf (occupancy-table game) (copy-occupancy-table (occupancy-table tile-map)))
    (occupy-outer-borders (occupancy-table game) dims)
    (setf (map-br-cell game) (map-position-to-cell dims))))

;;;; Occupancy Table

(defconstant unit-dim 16)

(defconstant unit-dim-1 (1- unit-dim))

(defun map-position-to-cell (position)
  (truncate-point position unit-dim))

(defun cell-to-map-position (cell)
  (* cell unit-dim))

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
                                               (+ px unit-dim-1)
                                               (+ py unit-dim-1)
                                               #xFF #x00 #x00 #x1F))))))))))

(defun occupy-outer-borders (occupancy-table map-dimensions)
  (destructure-point (mw mh) (map-position-to-cell map-dimensions)
    (occupy occupancy-table (point -1 -1) t)
    (dotimes (x (1+ mw))
      (occupy occupancy-table (point x -1) t)
      (occupy occupancy-table (point x mh) t))
    (dotimes (y mh)
      (occupy occupancy-table (point -1 y) t)
      (occupy occupancy-table (point mw y) t))))

(defun copy-occupancy-table (occupancy-table)
  (alexandria:copy-hash-table occupancy-table))

(defun list-unoccupied-cells (game)
  (let ((br-cell (map-br-cell game))
        (cells '()))
    (dotimes (y (y br-cell))
      (dotimes (x (x br-cell))
        (let ((cell (point x y)))
          (unless (occupiedp game cell)
            (push cell cells)))))
    cells))

(defun random-unoccupied-cell (game)
  (alexandria:random-elt (list-unoccupied-cells game)))

;;;; Game events

(defclass move-event (standard-event)
  ((units :initarg :units :reader units)
   (destination :initarg :destination :reader destination)))

(defmethod process-event ((game zonquerer) (event move-event) dt)
  (declare (ignore dt))
  (let ((destination (destination event)))
    (dolist (unit (units event))
      (setf (action unit)
            (list :move destination)))))

(defclass die-event (standard-event)
  ((units :initarg :units :reader units)))

(defmethod process-event ((game zonquerer) (event die-event) dt)
  (declare (ignore dt))
  (dolist (unit (units event))
    (setf (action unit)
          (list :die))))

(defclass attack-event (standard-event)
  ((victims :initarg :victims :reader victims)
   (attacker :initarg :attacker :reader attacker)))

(defmethod process-event ((game zonquerer) (event attack-event) dt)
  (declare (ignore dt))
  (let ((attacker (attacker event))
        (victims (victims event)))
    (when (null (action attacker))
      (setf (action attacker) (list :attack victims)))))

;;;; Timers

(defclass timer ()
  ((expiry :initarg :expiry :accessor expiry)
   (interval :initarg :interval :accessor interval)
   (callback :initarg :callback :reader callback)))

(defun remove-timer (game timer)
  (setf (timers game) (remove timer (timers game) :count 1)))

(defun add-timer (game &key absolute relative interval callback)
  (let ((timer (make-instance 'timer
                              :callback callback
                              :interval interval
                              :expiry (or absolute
                                          (+ (time-elapsed game) relative)))))
    (push timer (timers game))
    timer))

(defun update-timers (game dt)
  (let ((elapsed (+ (time-elapsed game) dt))
        (timers (timers game)))
    (setf (time-elapsed game) elapsed)
    (dolist (timer timers)
      (when (>= elapsed (expiry timer))
        (funcall (callback timer))
        (let ((interval (interval timer)))
          (if (null interval)
              (remove-timer game timer)
              (setf (expiry timer) (+ elapsed interval))))))))

;;;; Units

(defconstant unit-center
  (point (truncate unit-dim 2)
         (truncate unit-dim 2)))

(defconstant walk-speed-min 20)

(defvar *walk-speed-initial* 30)

(defconstant walk-speed-max 50)

(defvar *cooldown-initial* 2.0)

(defvar *hitpoints-initial* 50)

(defvar *damage-range* (list 4 8))

(defclass unit ()
  ((game :initarg :game :reader game)
   (position-on-map-float :initarg :position-on-map :accessor position-on-map-float)
   (facing :initform :down :accessor facing)
   (state :initform :become :accessor state)
   (selected :initform nil :accessor selectedp)
   (animator :initform nil :accessor animator)
   (anim-count :initform 0 :accessor anim-count)
   (anim :initform nil :accessor anim)
   (action :initform nil :accessor action)
   (path-to-walk :initform '() :accessor path-to-walk)
   (path-to-walk-blocked-cells :initform nil :accessor path-to-walk-blocked-cells)
   (walk-speed :initform *walk-speed-initial* :accessor walk-speed)
   (sheet-name :reader sheet-name)
   (cooldown :initform 0.0 :accessor cooldown)
   (attack-victims :initform '() :accessor attack-victims)
   (attack-anims :initform '() :accessor attack-anims)
   (hitpoints :initform *hitpoints-initial* :accessor hitpoints)))

(defclass player-unit (unit)
  ((sheet-name :initform :blueunit)))

(defclass enemy-unit (unit)
  ((sheet-name :initform :redunit)
   (ai-state :initform :wander :accessor ai-state)))

(defun position-on-map (unit)
  (truncate-point (position-on-map-float unit)))

(defun position-on-screen (unit)
  (- (position-on-map unit)
     (map-start-position (game unit))))

(defmethod initialize-instance :after ((unit unit) &key)
  (let* ((game (game unit))
         (name (sheet-name unit))
         (sprite-sheet (intern-resource game 'sprite-sheet name))
         (animator (external sprite-sheet)))
    (setf (animator unit) animator)
    (reset-animation unit)
    (unit-occupy unit)))

(defun unit-occupancy-cells (map-position)
  (list (map-position-to-cell map-position)))

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

(defun reset-animation (unit)
  (let ((tag (list (state unit) (facing unit))))
    (setf (anim-count unit) 0)
    (setf (anim unit)
          (funcall (animator unit) tag
                   :on-last-frame (lambda () (incf (anim-count unit)))))))

(defmethod (setf state) :around (new-state (unit unit))
  (let ((old-state (state unit)))
    (prog1 (call-next-method)
      (unless (eq old-state new-state)
        (reset-animation unit)))))

(defmethod (setf facing) :around (new-facing (unit unit))
  (let ((old-facing (facing unit)))
    (prog1 (call-next-method)
      (unless (eq old-facing new-facing)
        (reset-animation unit)))))

(defun unoccupied-adjacent-cells (unit)
  (let ((cell (map-position-to-cell (position-on-map unit))))
    (loop for direction in *taxicab-directions*
          for adjacent-cell = (+ cell direction)
          for adjacent-cell-position = (cell-to-map-position adjacent-cell)
          when (unit-can-occupy-p unit adjacent-cell-position)
          collect adjacent-cell)))

(defun adjacent-units (unit &optional (predicate (constantly t)))
  (let ((cell (map-position-to-cell (position-on-map unit)))
        (game (game unit)))
    (loop for direction in *taxicab-directions*
          for adjacent-cell = (+ cell direction)
          for occupant = (occupiedp game adjacent-cell)
          when (and occupant (funcall predicate occupant))
          collect occupant)))

(defun unit-do-move (unit goal-position-cell)
  (let ((game (game unit)))
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
        (setf goal-position-cell near)))
    (setf (path-to-walk unit)
          (compute-path-to-walk (map-position-to-cell (position-on-map unit))
                                goal-position-cell
                                (list (occupancy-table game)
                                      (path-to-walk-blocked-cells unit))))
    (setf (state unit) :walk)))

(defun move-to-unoccupied-adjacent-cell (unit)
  ;; This function is part of the unit walking mechanism; it is called
  ;; when the unit cannot stay in its current position.
  (let ((candidates (unoccupied-adjacent-cells unit)))
    (if (null candidates)
        ;; The unit is stuck in a bad place.  Time to die.
        (setf (action unit) (list :die))
        (setf (path-to-walk unit) (list (first candidates))))))

(defun unit-walk (unit dt)
  (let* ((game (game unit))
         (next-cell (car (path-to-walk unit)))
         (next-position (and next-cell (cell-to-map-position next-cell))))
    (cond ((null next-cell)
           (cond ((unit-can-occupy-p unit (position-on-map unit))
                  (unit-occupy unit)
                  (setf (facing unit) :down)
                  (setf (state unit) :idle))
                 (t
                  (move-to-unoccupied-adjacent-cell unit))))
          ((= next-position (position-on-map unit))
           (pop (path-to-walk unit)))
          ((unit-can-occupy-p unit next-position)
           (setf (state unit) :walk)
           (let ((direction (signum-point (- next-position (position-on-map unit)))))
             (setf (facing unit)
                   (case direction
                     (#C(0 1) :down)
                     (#C(1 0) :right)
                     (#C(-1 0) :left)
                     (#C(0 -1) :up)
                     (t (facing unit))))
             (incf (position-on-map-float unit)
                   (* dt
                      (walk-speed unit)
                      direction))))
          (t
           ;; Mark next position's cell as blocked and search for a
           ;; new path.
           (let ((target-cell (car (last (path-to-walk unit)))))
             (setf (gethash next-cell (path-to-walk-blocked-cells unit)) t)
             (setf (path-to-walk unit)
                   (compute-path-to-walk (map-position-to-cell (position-on-map unit))
                                         target-cell
                                         (list (occupancy-table game)
                                               (path-to-walk-blocked-cells unit))))
             (when (null (path-to-walk unit))
               (cond ((unit-can-occupy-p unit (position-on-map unit))
                      (push (map-position-to-cell (position-on-map unit))
                            (path-to-walk unit)))
                     (t
                      ;; There is no path to the target position,
                      ;; and we can't occupy the current
                      ;; position, so find an adjacent cell to
                      ;; move to.
                      (move-to-unoccupied-adjacent-cell unit)))))))))

(defun enemy-of (unit)
  (let ((us (class-of unit)))
    (lambda (occupant)
      (and (typep occupant 'unit)
           (let ((them (class-of occupant)))
             (not (eq us them)))))))

(defgeneric unit-idle (unit dt)
  (:method-combination or :most-specific-last))

(defmethod unit-idle or ((unit unit) dt)
  (declare (ignore dt))
  (when (and (zerop (cooldown unit))
             (null (action unit)))
    (let ((victims (adjacent-units unit (enemy-of unit))))
      (when victims
        (push-event (game unit)
                    (make-instance 'attack-event
                                   :victims victims
                                   :attacker unit))
        t))))

(defmethod unit-idle or ((unit enemy-unit) dt)
  (declare (ignore dt))
  (when (null (action unit))
    (case (ai-state unit)
      (:wander
       (let ((cells (unoccupied-adjacent-cells unit)))
         (when cells
           (setf (action unit)
                 (list :move (alexandria:random-elt cells)))
           t))))))

(defun update-unit (unit dt)
  (setf (cooldown unit) (max 0.0 (- (cooldown unit) dt)))
  (when (and (not (plusp (hitpoints unit)))
             (not (eq (state unit) :dead)))
    (setf (facing unit) :down)
    (setf (state unit) :die)
    (setf (action unit) nil))
  (case (car (action unit))
    (:move
     (unit-do-move unit (cadr (action unit)))
     (setf (action unit) nil))
    (:die
     (setf (facing unit) :down)
     (setf (state unit) :die)
     (setf (action unit) nil))
    (:attack
     (setf (cooldown unit) *cooldown-initial*)
     (setf (attack-victims unit) (cadr (action unit)))
     (setf (facing unit) :down)
     (setf (state unit) :attack)
     (setf (action unit) nil)))
  (case (state unit)
    (:walk
     (unit-walk unit dt))
    (:become
     (when (plusp (anim-count unit))
       (setf (state unit) :idle)))
    (:die
     (setf (selectedp unit) nil)
     (when (plusp (anim-count unit))
       (setf (state unit) :dead)
       (when (unit-can-occupy-p unit (position-on-map unit))
         (unit-occupy unit nil))
       (sort-units (game unit))
       (check-end-of-game (game unit))))
    (:dead)
    (:idle
     (unit-idle unit dt))
    (:attack
     (when (plusp (anim-count unit))
       (setf (state unit) :idle)
       (let* ((game (game unit))
              (animator (external (intern-resource game 'sprite-sheet :burn))))
         (dolist (victim (attack-victims unit))
           (let ((victim victim)
                 (anim nil))
             (setq anim
                   (funcall animator (list :burn)
                            :on-last-frame
                            (lambda ()
                              (setf (attack-anims victim)
                                    (remove anim (attack-anims victim) :count 1))
                              (decf (hitpoints victim) (random-in-range *damage-range*)))))
             (push anim (attack-anims victim))))
         (setf (attack-victims unit) '()))))))

(defun compute-path-to-walk (start-cell goal-cell occupancy-tables)
  (flet ((blockedp (cell)
           (some (lambda (table) (gethash cell table)) occupancy-tables)))
    (if (or (blockedp start-cell) (blockedp goal-cell))
        '()
        (reverse
         (a-star start-cell
                 :goal-state-p (lambda (cell)
                                 (= cell goal-cell))
                 :heuristic (lambda (cell)
                              (abs (- goal-cell cell)))
                 :failure-p #'blockedp
                 :max-cost (* 5 (abs (- goal-cell start-cell)))
                 :expand (lambda (cell)
                           (loop for direction in *taxicab-directions*
                                 for new-cell = (+ cell direction)
                                 collect (list (abs (- new-cell cell))
                                               new-cell))))))))

(defun draw-unit (unit dt)
  (let ((pos (position-on-screen unit))
        (game (game unit)))
    (funcall (anim unit) :draw pos dt)
    (dolist (anim (attack-anims unit))
      (funcall anim :draw pos dt))
    (when (selectedp unit)
      (sdl2-ffi.functions:rectangle-rgba (renderer game)
                                         (1- (x pos))
                                         (1- (y pos))
                                         (+ 1 unit-dim (x pos))
                                         (+ 1 unit-dim (y pos))
                                         #xFF #xFF #xFF #x7F))
    (when (debuggingp game)
      (dolist (cell (path-to-walk unit))
        (let ((spos (cell-to-screen-position game cell)))
          (sdl2-ffi.functions:pixel-rgba (renderer game)
                                         (x spos)
                                         (y spos)
                                         #x00 #x00 #x00 #x7F))))))

(defun add-unit (unit)
  (pushnew unit (units (game unit))))

(defun create-player-units (game)
  (let ((center (+ (map-start-position game) (/ (video-dimensions game) 2))))
    (dotimes (i 9)
      (multiple-value-bind (c r) (truncate i 3)
        (let* ((pos (+ center
                       (point (* (1- c) unit-dim)
                              (* (1- r) unit-dim))))
               (unit (make-instance 'player-unit
                                    :game game
                                    :position-on-map pos)))
          (add-timer game
                     :absolute (* (1+ i) 0.2)
                     :callback (lambda ()
                                 (add-unit unit))))))))

(defun create-enemy-units (game)
  (let ((cells (alexandria:shuffle (list-unoccupied-cells game))))
    (dotimes (i 6)
      (let* ((cell (pop cells))
             (pos (cell-to-map-position cell))
             (unit (make-instance 'enemy-unit
                                  :game game
                                  :position-on-map pos)))
        (add-timer game
                   :absolute (* (1+ i) 0.2)
                   :callback (lambda ()
                               (add-unit unit)))))))

(defun create-enemy-unit (game)
  (let* ((cell (random-unoccupied-cell game))
         (pos (cell-to-map-position cell))
         (unit (make-instance 'enemy-unit
                              :game game
                              :position-on-map pos)))
    (add-unit unit)))

(defun setup-units (game)
  (create-player-units game)
  (create-enemy-units game)
  (add-timer game
             :absolute 60
             :interval 60
             :callback (lambda ()
                         (unless (end-game-anim game)
                           (create-enemy-unit game)))))

(defun selectablep (unit)
  (and (typep unit 'player-unit)
       (not (member (state unit) '(:die :dead)))))

(defun selected-units (game)
  (remove-if-not #'selectedp (units game)))

(defun sort-units (game)
  (setf (units game) (sort (units game) #'unit<)))

(defun unit< (unit1 unit2)
  (let ((state1 (state unit1))
        (state2 (state unit2)))
    (cond ((eq state1 :dead) (not (eq state2 :dead)))
          ((eq state2 :dead) nil)
          (t nil))))

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
    (labels ((inside-bounds-p (point)
               (and (<= left (+ (x point) unit-dim-1) (+ right unit-dim-1))
                    (<= top (+ (y point) unit-dim-1) (+ bottom unit-dim-1))))
             (in-selection-p (unit)
               (and (inside-bounds-p (position-on-map unit))
                    (selectablep unit))))
      (remove-if-not #'in-selection-p (units game)))))

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

(defvar *digit-scancodes*
  (list :scancode-1 :scancode-2 :scancode-3))

(defun update-stored-selections (game dt)
  (declare (ignore dt))
  (let ((keys (keys game))
        (stored-selections (stored-selections game)))
    (dolist (scancode *digit-scancodes*)
      (when (member scancode keys)
        (let ((store (member :scancode-lctrl keys))
              (selected (selected-units game)))
          (symbol-macrolet ((stored-selected (gethash scancode stored-selections)))
            (cond ((and store selected)
                   (setf stored-selected selected)
                   (return-from update-stored-selections))
                  ((and (not store) stored-selected)
                   (dolist (unit selected)
                     (setf (selectedp unit) nil))
                   (dolist (unit stored-selected)
                     (when (selectablep unit)
                       (setf (selectedp unit) t)))
                   (return-from update-stored-selections)))))))))

;;;; End Game

(defun end-of-game-p (game)
  (let ((player-alive 0)
        (enemy-alive 0))
    (dolist (unit (units game))
      (unless (eq (state unit) :dead)
        (if (typep unit 'player-unit)
            (incf player-alive)
            (incf enemy-alive))))
    (cond ((zerop enemy-alive) :win)
          ((zerop player-alive) :lose)
          (t nil))))

(defun reveal (secret)
  (map 'string
       (lambda (char)
         (if (alpha-char-p char)
             (code-char (+ (char-code #\A) (mod (- (char-code char) 3) 26)))
             char))
       secret))

(defun check-end-of-game (game)
  (let ((end (end-of-game-p game)))
    (when end
      (dolist (unit (units game))
        (setf (selectedp unit) nil))
      (setf (end-game-anim game)
            (funcall (external (intern-resource game 'sprite-sheet :blueunit))
                     (list (if (eq end :win) :attack :die) :down)
                     :on-last-frame (lambda ()
                                      (funcall (end-game-anim game) :pause))))
      (setf (end-game-strings game)
            (mapcar #'reveal
                    (list (if (eq end :win)
                              "JXU FEMUH EV BYIF SECFYBUI OEK!"
                              "OEK XQLU HUQSXUT JXU UDT EV JXU BYIJ!")
                          "JXYI YI PEDGKUHUH RO TUQJX"
                          "IFHYDW BYIF WQCU ZQC 2021"
                          "JOFU UIS JE UNYJ, IFQSU JE FBQO QWQYD"))))))

(defun draw-end-game (game dt)
  (let* ((screen-center (truncate-point (video-dimensions game) 2))
         (anim-pos (- screen-center (* 10 unit-center))))
    (funcall (end-game-anim game) :draw anim-pos dt 8)
    (loop for string in (end-game-strings game)
          for xd = (* -2 (length string))
          for yd = (* 3 unit-dim) then (+ yd 6)
          for text-pos = (+ screen-center (point xd yd))
          do (funcall (string-renderer game) string text-pos))))

;;;; The game

(defmethod update ((game zonquerer) dt)
  (when (member :scancode-d (keys game))
    (setf (debuggingp game)
          (not (member :scancode-lshift (keys game)))))
  (update-stored-selections game dt)
  (update-map game dt)
  (update-timers game dt)
  (dolist (unit (units game))
    (update-unit unit dt))
  (when (member :scancode-k (keys game))
    (push-event game (make-instance 'die-event :units (selected-units game))))
  (when (and (end-game-anim game)
             (member :scancode-space (keys game)))
    (setf (play-again-p game) t)
    (sdl2:push-quit-event)))

(defmethod draw ((game zonquerer) dt)
  (draw-map game dt)
  (when (debuggingp game)
    (draw-occupancy-table game dt))
  (dolist (unit (units game))
    (draw-unit unit dt))
  (when (selection-event-buildup game)
    (draw-selection game dt))
  (when (end-game-anim game)
    (draw-end-game game dt)))

(defmethod game-loop :before ((game zonquerer))
  (setup-cursors game)
  (setup-map game)
  (setup-units game)
  (setup-font game))

(defmethod mouse-event ((game zonquerer) (state (eql :down)) (button (eql 1)) position)
  (when (end-game-anim game)
    (return-from mouse-event))
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
  (when (end-game-anim game)
    (return-from mouse-event))
  (let ((selection-event (selection-event-buildup game)))
    (cond (selection-event
           (setf (end-position selection-event)
                 (+ (map-start-position game) position))
           (push-event game selection-event)
           (setf (selection-event-buildup game) nil)))))

(defmethod mouse-event ((game zonquerer) (state (eql :down)) (button (eql 3)) position)
  (push-event game
              (make-instance 'move-event
                             :units (selected-units game)
                             :destination (map-position-to-cell
                                           (+ (map-start-position game) position)))))

(defun play ()
  (loop for game = (driver (make-instance 'zonquerer))
        while (play-again-p game)
        finally (return game)))
