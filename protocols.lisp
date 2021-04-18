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
   #:video-dimensions
   #:keys
   #:mouse-position
   #:mouse-event
   #:request-cursor
   #:game-loop
   #:update
   #:draw
   #:find-resource
   #:create-resource
   #:free-all-resources
   #:remove-resource
   #:assets-directory
   #:push-event
   #:process-event
   #:resource
   #:game
   #:key
   #:external
   #:who-depends
   #:depends-on
   #:depend-on
   #:free
   #:event))

(in-package #:zonquerer/protocols)

(defclass game ()
  ())

(defgeneric renderer (game))

(defgeneric video-dimensions (game))

(defgeneric keys (game))

(defgeneric mouse-position (game))

(defgeneric mouse-event (game state button position))

(defgeneric request-cursor (game name))

(defgeneric game-loop (game))

(defgeneric update (game dt))

(defgeneric draw (game dt))

(defgeneric find-resource (game kind name))

(defgeneric (setf find-resource) (resource game kind name))

(defgeneric create-resource (game kind name &key &allow-other-keys))

(defgeneric free-all-resources (game))

(defgeneric remove-resource (game kind name))

(defgeneric assets-directory (game))

(defgeneric push-event (game event))

(defgeneric process-event (game event dt))

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

(defclass event ()
  ())
