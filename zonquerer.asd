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

;;;; System definitions

;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:register-system-packages :sdl2 '(:sdl2-ffi.functions))
(asdf:register-system-packages :cl-autowrap '(:autowrap))
(asdf:register-system-packages :cl-plus-c '(:plus-c))

(asdf:defsystem #:zonquerer
  :description ""
  :author "death <github.com/death>"
  :license "AGPL3"
  :class :package-inferred-system
  :defsystem-depends-on ("asdf-package-system" "deploy")
  :depends-on ("zonquerer/all")
  :build-operation "deploy-op"
  :build-pathname "zonquerer"
  :entry-point "zonquerer:play")
