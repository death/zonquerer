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

(defpackage #:zonquerer/resources
  (:use
   #:cl
   #:zonquerer/protocols
   #:zonquerer/utils)
  (:import-from
   #:pngload)
  (:import-from
   #:sdl2)
  (:export
   #:standard-resource
   #:image
   #:surface
   #:cursor
   #:texture
   #:tile-set
   #:tile-map
   #:dimensions
   #:occupancy-table
   #:sprite-sheet
   #:font))

(in-package #:zonquerer/resources)

;;;; Standard Resource

(defclass standard-resource (resource)
  ((game :initarg :game :reader game)
   (name :initarg :name :reader name)
   (who-depends :initform '() :accessor who-depends)
   (depends-on :initform '() :accessor depends-on)
   (state :initform :allocated :accessor state))
  (:default-initargs
   :game (error "Initarg :game is required.")
   :name (error "Initarg :name is required.")))

(defmethod depend-on ((resource1 standard-resource) (resource2 standard-resource))
  (assert (eq (state resource1) :allocated))
  (assert (eq (state resource2) :allocated))
  (pushnew resource1 (who-depends resource2))
  (pushnew resource2 (depends-on resource1)))

(defmethod free :around ((resource standard-resource))
  (case (state resource)
    (:allocated
     (setf (state resource) :freeing)
     (dolist (dependent (who-depends resource))
       (free dependent))
     (call-next-method)
     (setf (state resource) :deallocated)
     (setf (who-depends resource) '())
     (setf (depends-on resource) '())
     (remove-resource (game resource) (kind resource) (name resource)))))

(defmethod free ((resource standard-resource))
  ;; Do nothing.
  )

(defmethod external ((resource standard-resource))
  ;; The external is the resource itself.
  resource)

(defmethod kind ((resource standard-resource))
  (class-name (class-of resource)))

;;;; Image

(defclass image (standard-resource)
  ((png :initarg :png :reader external)))

(defmethod create-resource ((game game) (kind (eql 'image)) name &key)
  (let ((filename (make-asset-filename game name "png")))
    (make-instance 'image
                   :name name
                   :game game
                   :png (pngload:load-file filename :flatten t :static-vector t))))

(defmethod free ((image image))
  (static-vectors:free-static-vector (pngload:data (external image))))

;;;; Surface

(defclass surface (standard-resource)
  ((sdl-surface :initarg :sdl-surface :reader external)))

(defmethod create-resource ((game game) (kind (eql 'surface)) name &key)
  (let* ((image (intern-resource game 'image name))
         (png (external image))
         (data (pngload:data png))
         (pointer (static-vectors:static-vector-pointer data))
         (width (pngload:width png))
         (height (pngload:height png))
         (bit-depth (check (pngload:bit-depth png) 8))
         (color-type (check (pngload:color-type png) :truecolour-alpha))
         (pitch (* 4 width))
         (depth (* 4 bit-depth))
         (sdl-surface (sdl2:create-rgb-surface-with-format-from
                       pointer width height depth pitch
                       :format sdl2:+pixelformat-rgba32+))
         (surface (make-instance 'surface
                                 :name name
                                 :game game
                                 :sdl-surface sdl-surface)))
    (declare (ignore color-type))
    (depend-on surface image)
    surface))

(defmethod free ((surface surface))
  (sdl2:free-surface (external surface)))

;;;; Cursor

(defclass cursor (standard-resource)
  ((sdl-cursor :initarg :sdl-cursor :reader external)))

(defmethod create-resource ((game game) (kind (eql 'cursor)) name &key (hot #C(16 16)))
  (destructure-point (hx hy) hot
    (let* ((surface (intern-resource game 'surface name))
           (sdl-surface (external surface))
           (sdl-cursor (sdl2-ffi.functions:sdl-create-color-cursor sdl-surface hx hy))
           (cursor (make-instance 'cursor
                                  :name name
                                  :game game
                                  :sdl-cursor sdl-cursor)))
      (depend-on cursor surface)
      cursor)))

(defmethod free ((cursor cursor))
  (sdl2-ffi.functions:sdl-free-cursor (external cursor)))

;;;; Texture

(defclass texture (standard-resource)
  ((sdl-texture :initarg :sdl-texture :reader external)))

(defmethod create-resource ((game game) (kind (eql 'texture)) name &key)
  (let* ((surface (intern-resource game 'surface name))
         (sdl-surface (external surface))
         (sdl-texture (sdl2:create-texture-from-surface (renderer game) sdl-surface))
         (texture (make-instance 'texture
                                 :name name
                                 :game game
                                 :sdl-texture sdl-texture)))
    ;; Note that the texture does not depend on the surface.
    texture))

(defmethod free ((texture texture))
  (sdl2:destroy-texture (external texture)))

;;;; Tile Set

(defclass tile-set (standard-resource)
  ((columns :initarg :columns :reader columns)
   (tile-width :initarg :tile-width :reader tile-width)
   (tile-height :initarg :tile-height :reader tile-height)
   (tile-count :initarg :tile-count :reader tile-count)
   (tile-props :initarg :tile-props :reader tile-props)
   (render-function :initform nil :accessor external)))

(defmethod create-resource ((game game) (kind (eql 'tile-set)) name &key)
  (let* ((filename (make-asset-filename game name "json"))
         (json (read-json-file filename))
         (columns (gethash "columns" json))
         (tile-width (gethash "tilewidth" json))
         (tile-height (gethash "tileheight" json))
         (tile-count (gethash "tilecount" json))
         (tiles-json (gethash "tiles" json))
         (tile-props (make-array tile-count :initial-element '()))
         (image-name (remove-suffix (gethash "image" json) ".png"))
         (texture (intern-resource game 'texture image-name))
         (tile-set (make-instance 'tile-set
                                  :name name
                                  :game game
                                  :columns columns
                                  :tile-width tile-width
                                  :tile-height tile-height
                                  :tile-count tile-count
                                  :tile-props tile-props)))
    (depend-on tile-set texture)
    (loop for tile-json across tiles-json
          for id = (gethash "id" tile-json)
          for props-json = (gethash "properties" tile-json)
          for props = (make-hash-table)
          do (loop for prop across props-json
                   for key = (gethash "name" prop)
                   for value = (gethash "value" prop)
                   do (setf (gethash (as-keyword key) props) value))
             (setf (aref tile-props id) props))
    (let ((renderer (renderer game))
          (sdl-texture (external texture))
          (sr (sdl2:make-rect 0 0 tile-width tile-height))
          (dr (sdl2:make-rect 0 0 tile-width tile-height)))
      (setf (external tile-set)
            (lambda (n dest-pos)
              (assert (< n tile-count))
              (multiple-value-bind (row col) (truncate n columns)
                (setf (sdl2:rect-x sr) (* col tile-width))
                (setf (sdl2:rect-y sr) (* row tile-height))
                (setf (sdl2:rect-x dr) (x dest-pos))
                (setf (sdl2:rect-y dr) (y dest-pos))
                (sdl2:render-copy renderer sdl-texture :source-rect sr :dest-rect dr)))))
    tile-set))

(defun tile-property (tile-set n key &optional (default nil))
  (let ((props (aref (tile-props tile-set) n)))
    (if props
        (gethash key props default)
        default)))

;;;; Tile Map

(defclass tile-map (standard-resource)
  ((dimensions :initform #C(0 0) :accessor dimensions)
   (occupancy-table :initarg :occupancy-table :reader occupancy-table)
   (render-function :initform nil :accessor external)
   (sdl-texture :initform nil :accessor sdl-texture)))

(defmethod create-resource ((game game) (kind (eql 'tile-map)) name &key)
  (let* ((filename (make-asset-filename game name "json"))
         (json (read-json-file filename))
         (width (gethash "width" json))
         (height (gethash "height" json))
         (layers-json (gethash "layers" json))
         (tile-sets-json (gethash "tilesets" json))
         (tile-sets '())
         (layers '())
         (occupancy-table (make-hash-table))
         (tile-map (make-instance 'tile-map
                                  :name name
                                  :game game
                                  :occupancy-table occupancy-table)))
    (assert (= (length tile-sets-json) (length layers-json)))
    (dotimes (i (length tile-sets-json))
      (let* ((tile-set-json (aref tile-sets-json i))
             (layer-json (aref layers-json i))
             (source (remove-suffix (gethash "source" tile-set-json) ".json"))
             (first-gid (gethash "firstgid" tile-set-json))
             (tile-set (intern-resource game 'tile-set source))
             (data (gethash "data" layer-json)))
        (assert (= (length data) (* width height)))
        (push tile-set tile-sets)
        (push (map 'list (lambda (x) (- x first-gid)) data) layers)
        ;; We don't need to depend on the tile sets because we render
        ;; them ahead of time to our own texture.
        ))
    (setf tile-sets (nreverse tile-sets))
    (setf layers (nreverse layers))
    (let* ((renderer (renderer game))
           (tile-renderers (mapcar #'external tile-sets))
           (tile-widths (mapcar #'tile-width tile-sets))
           (tile-heights (mapcar #'tile-height tile-sets))
           (width-in-pixels (* width (tile-width (first tile-sets))))
           (height-in-pixels (* height (tile-height (first tile-sets))))
           (sdl-texture (sdl2:create-texture renderer
                                             sdl2:+pixelformat-rgba32+
                                             :target
                                             width-in-pixels
                                             height-in-pixels)))
      (setf (dimensions tile-map) (point width-in-pixels height-in-pixels))
      (setf (sdl-texture tile-map) sdl-texture)
      (sdl2:set-render-target renderer sdl-texture)
      (loop for tile-set in tile-sets
            for layer in layers
            for tw in tile-widths
            for th in tile-heights
            for render-tile in tile-renderers
            do (dotimes (row height)
                 (let ((pos (point 0 (* row th))))
                   (dotimes (col width)
                     (let* ((n (pop layer))
                            (occupy (tile-property tile-set n :occupy)))
                       (when (eq occupy :true)
                         (setf (gethash (point col row) occupancy-table) occupy))
                       (funcall render-tile n pos)
                       (incf pos (point tw 0)))))))
      (sdl2:set-render-target renderer nil)
      (destructure-point (w h) (video-dimensions game)
        (let ((source-rect (sdl2:make-rect 0 0 w h))
              (dest-rect (sdl2:make-rect 0 0 w h)))
          (setf (external tile-map)
                (lambda (start-pos)
                  (setf (sdl2:rect-x source-rect) (x start-pos))
                  (setf (sdl2:rect-y source-rect) (y start-pos))
                  (setf (sdl2:rect-width dest-rect)
                        (min w (- width-in-pixels (x start-pos))))
                  (setf (sdl2:rect-height dest-rect)
                        (min h (- width-in-pixels (y start-pos))))
                  (sdl2:render-copy renderer sdl-texture
                                    :source-rect source-rect
                                    :dest-rect dest-rect))))))
    tile-map))

(defmethod free ((tile-map tile-map))
  (sdl2:destroy-texture (sdl-texture tile-map)))

;;;; Sprite Sheet

;; Some assumptions:
;;
;; - All frames in the sprite sheet are tagged.
;;
;; - The "frames" field is a JSON object (not an array), and each key
;;   is of the form "{tag} {frame}".

(defclass sprite-sheet (standard-resource)
  ((frames :initarg :frames :reader frames)
   (anim-function :initform nil :accessor external)))

(defun make-tag (tag-json)
  (make-keyword-list tag-json))

(defun make-frames (frames-json frame-tags-json)
  (let ((frames (make-hash-table :test 'equal)))
    (loop for frame-tag-json across frame-tags-json
          for tag-json = (gethash "name" frame-tag-json)
          for tag = (make-tag tag-json)
          for from = (gethash "from" frame-tag-json)
          for to = (gethash "to" frame-tag-json)
          for count = (1+ (- to from))
          for rects = (make-array count)
          for durations = (make-array count)
          do (do ((i from (1+ i)))
                 ((> i to))
               (let* ((key (format nil "~A ~D" tag-json i))
                      (frame-json (gethash key frames-json))
                      (rect-json (gethash "frame" frame-json)))
                 (setf (aref rects (- i from))
                       (sdl2:make-rect (gethash "x" rect-json)
                                       (gethash "y" rect-json)
                                       (gethash "w" rect-json)
                                       (gethash "h" rect-json)))
                 (setf (aref durations (- i from))
                       (gethash "duration" frame-json))))
             (setf (gethash tag frames)
                   (list rects durations)))
    frames))

(defmethod create-resource ((game game) (kind (eql 'sprite-sheet)) name &key)
  (let* ((filename (make-asset-filename game name "json"))
         (json (read-json-file filename))
         (meta-json (gethash "meta" json))
         (frames-json (gethash "frames" json))
         (image-name (remove-suffix (gethash "image" meta-json) ".png"))
         (frame-tags-json (gethash "frameTags" meta-json))
         (frames (make-frames frames-json frame-tags-json))
         (texture (intern-resource game 'texture image-name))
         (sprite-sheet (make-instance 'sprite-sheet
                                      :name name
                                      :game game
                                      :frames frames)))
    (depend-on sprite-sheet texture)
    (setf (external sprite-sheet)
          (lambda (tag &key (start-frame 0) (on-last-frame #'noop))
            (sprite-animator sprite-sheet tag start-frame on-last-frame)))
    sprite-sheet))

(defun sprite-animator (sprite-sheet tag start-frame last-frame-callback)
  (destructuring-bind (rects durations)
      (gethash tag (frames sprite-sheet))
    (let* ((texture (first (depends-on sprite-sheet)))
           (sdl-texture (external texture))
           (renderer (renderer (game sprite-sheet)))
           (time-ms 0.0)
           (num-frames (length rects))
           (frame (mod start-frame num-frames))
           (source-rect (aref rects frame))
           (wait-ms (aref durations frame))
           (dest-rect (sdl2:copy-rect source-rect))
           (paused nil))
      (lambda (op &optional dest-pos dt (scale 1))
        (ecase op
          (:pause
           (setf paused t))
          (:continue
           (setf paused nil))
          (:draw
           (let* ((dt-ms (* dt 1000.0))
                  (now-ms (+ time-ms dt-ms)))
             (loop while (< (+ time-ms wait-ms) now-ms)
                   do (incf time-ms wait-ms)
                      (let ((new-frame (if paused frame (mod (1+ frame) num-frames))))
                        (when (and (not paused) (= new-frame (1- num-frames)))
                          (funcall last-frame-callback))
                        (setf frame new-frame)
                        (setf source-rect (aref rects frame))
                        (setf wait-ms (aref durations frame))))
             (decf wait-ms (- now-ms time-ms))
             (setf time-ms now-ms)
             (setf (sdl2:rect-width dest-rect) (* scale (sdl2:rect-width source-rect)))
             (setf (sdl2:rect-height dest-rect) (* scale (sdl2:rect-height source-rect)))
             (setf (sdl2:rect-x dest-rect) (x dest-pos))
             (setf (sdl2:rect-y dest-rect) (y dest-pos))
             (sdl2:render-copy renderer
                               sdl-texture
                               :source-rect source-rect
                               :dest-rect dest-rect))))))))

;;;; Font

(defclass font (standard-resource)
  ((alphabet-table :initarg :alphabet-table :reader alphabet-table)
   (glyph-dimensions :initarg :glyph-dimensions :reader glyph-dimensions)
   (render-function :initform nil :accessor external)))

(defmethod create-resource ((game game) (kind (eql 'font)) name
                            &key (alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-+")
                                 (glyph-dimensions #C(8 10))
                                 (spacing 1))
  (let* ((texture (intern-resource game 'texture name))
         (alphabet-table (make-hash-table))
         (font (make-instance 'font
                              :name name
                              :game game
                              :alphabet-table alphabet-table
                              :glyph-dimensions glyph-dimensions)))
    (depend-on font texture)
    (destructure-point (gw gh) glyph-dimensions
      (loop for char across alphabet
            for i upfrom 0
            do (setf (gethash char alphabet-table)
                     (sdl2:make-rect (* i gw) 0 gw gh)))
      (let ((next-pos-in-line (point (+ (x glyph-dimensions) spacing) 0))
            (sdl-texture (external texture))
            (weird-chars (make-hash-table))
            (dest-rect (sdl2:make-rect 0 0 gw gh))
            (renderer (renderer game)))
        (setf (gethash #\Space weird-chars) t)
        (setf (external font)
              (lambda (string start-pos)
                (let ((pos start-pos)
                      (rect nil))
                  (loop for char across string
                        do (cond ((setq rect (gethash char alphabet-table))
                                  (setf (sdl2:rect-x dest-rect) (x pos))
                                  (setf (sdl2:rect-y dest-rect) (y pos))
                                  (sdl2:render-copy renderer sdl-texture
                                                    :source-rect rect
                                                    :dest-rect dest-rect))
                                 (t
                                  (unless (gethash char weird-chars)
                                    (warn "No glyph found in font ~S for character ~S." name char)
                                    (setf (gethash char weird-chars) t))))
                           (setf pos (+ pos next-pos-in-line))))))))
    font))
