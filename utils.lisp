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

(defpackage #:zonquerer/utils
  (:use
   #:cl
   #:zonquerer/protocols)
  (:export
   #:check
   #:intern-resource
   #:x
   #:y
   #:point
   #:destructure-point))

(in-package #:zonquerer/utils)

(defmacro check (form expected-value &optional (test 'eql))
  (let ((actual-value (gensym)))
    `(let ((,actual-value ,form))
       (assert (,test ,actual-value ,expected-value)
               (,actual-value)
               "Form ~S evaluated to ~S, which is not ~S to value ~S."
               ',form
               ,actual-value
               ',test
               ,expected-value)
       ,actual-value)))

(defun intern-resource (game kind name &rest keys)
  (or (find-resource game kind name)
      (setf (find-resource game kind name)
            (apply #'create-resource game kind name keys))))

(defun x (point)
  (realpart point))

(defun y (point)
  (imagpart point))

(defun point (x y)
  (complex x y))

(defmacro destructure-point ((x y) point &body body)
  (let ((var (gensym)))
    `(let* ((,var ,point)
            (,x (x ,var))
            (,y (y ,var)))
       ,@body)))
