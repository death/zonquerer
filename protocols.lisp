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

(defpackage #:zonquerer/protocols
  (:use #:cl)
  (:export
   #:game
   #:renderer
   #:update
   #:draw
   #:find-resource
   #:create-resource
   #:free-all-resources
   #:remove-resource
   #:assets-directory
   #:resource
   #:game
   #:key
   #:external
   #:who-depends
   #:depends-on
   #:depend-on
   #:free))

(in-package #:zonquerer/protocols)

(defclass game ()
  ())

(defgeneric renderer (game))

(defgeneric update (game))

(defgeneric draw (game))

(defgeneric find-resource (game kind name))

(defgeneric (setf find-resource) (resource game kind name))

(defgeneric create-resource (game kind name &key &allow-other-keys))

(defgeneric free-all-resources (game))

(defgeneric remove-resource (game kind name))

(defgeneric assets-directory (game))

(defclass resource ()
  ())

(defgeneric game (resource))

(defgeneric name (resource))

(defgeneric kind (resource))

(defgeneric external (resource))

(defgeneric who-depends (resource))

(defgeneric depends-on (resource))

(defgeneric depend-on (resource1 resource2))

(defgeneric free (resource))
