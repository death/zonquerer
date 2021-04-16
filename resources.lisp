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
   #:cursor))

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
  (let ((filename (merge-pathnames
                   (make-pathname :name (string-downcase name)
                                  :type "png")
                   (assets-directory game))))
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
