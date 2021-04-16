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
   #:render-map))

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
   (tile-count :initarg :tile-count :reader tile-count)))

(defmethod create-resource ((game game) (kind (eql 'tile-set)) name &key)
  (let* ((filename (make-asset-filename game name "json"))
         (json (read-json-file filename))
         (columns (gethash "columns" json))
         (tile-width (gethash "tilewidth" json))
         (tile-height (gethash "tileheight" json))
         (tile-count (gethash "tilecount" json))
         (image-name (remove-suffix (gethash "image" json) ".png"))
         (texture (intern-resource game 'texture image-name))
         (tile-set (make-instance 'tile-set
                                  :name name
                                  :game game
                                  :columns columns
                                  :tile-width tile-width
                                  :tile-height tile-height
                                  :tile-count tile-count)))
    (depend-on tile-set texture)
    tile-set))

(defun tile-renderer (tile-set)
  (let* ((tw (tile-width tile-set))
         (th (tile-height tile-set))
         (tc (tile-count tile-set))
         (cols (columns tile-set))
         (game (game tile-set))
         (renderer (renderer game))
         (texture (first (depends-on tile-set)))
         (sdl-texture (external texture)))
    (lambda (n dest-pos)
      (assert (< n tc))
      (multiple-value-bind (row col) (truncate n cols)
        (sdl2:with-rects ((sr (* col tw) (* row th) tw th)
                          (dr (x dest-pos) (y dest-pos) tw th))
          (sdl2:render-copy renderer sdl-texture :source-rect sr :dest-rect dr))))))

;;;; Tile Map

(defclass tile-map (standard-resource)
  ((num-rows :initarg :num-rows :reader num-rows)
   (num-cols :initarg :num-cols :reader num-cols)
   (tile-sets :initform '() :accessor tile-sets)
   (layers :initform '() :accessor layers)
   (dimensions :initform #C(0 0) :accessor dimensions)))

(defmethod create-resource ((game game) (kind (eql 'tile-map)) name &key)
  (let* ((filename (make-asset-filename game name "json"))
         (json (read-json-file filename))
         (width (gethash "width" json))
         (height (gethash "height" json))
         (layers-json (gethash "layers" json))
         (tile-sets-json (gethash "tilesets" json))
         (tile-sets '())
         (layers '())
         (tile-map (make-instance 'tile-map
                                  :name name
                                  :game game
                                  :num-cols width
                                  :num-rows height)))
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
        (depend-on tile-map tile-set)))
    (setf (tile-sets tile-map) (nreverse tile-sets))
    (setf (layers tile-map) (nreverse layers))
    (setf (dimensions tile-map)
          (point (* width (tile-width (first tile-sets)))
                 (* height (tile-height (first tile-sets)))))
    tile-map))

(defun render-map (tile-map start-pos)
  (loop with cols = (num-cols tile-map)
        with rows = (num-rows tile-map)
        for layer in (layers tile-map)
        for tile-set in (tile-sets tile-map)
        for tw = (tile-width tile-set)
        for th = (tile-height tile-set)
        for render-tile = (tile-renderer tile-set)
        do (dotimes (row rows)
             (let ((pos (point 0 (* row th))))
               (dotimes (col cols)
                 (let ((n (pop layer)))
                   (funcall render-tile n (- pos start-pos))
                   (incf pos (point tw 0))))))))
