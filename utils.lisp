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
  (:import-from
   #:alexandria)
  (:import-from
   #:split-sequence)
  (:import-from
   #:com.gigamonkeys.json)
  (:export
   #:check
   #:intern-resource
   #:x
   #:y
   #:point
   #:destructure-point
   #:truncate-point
   #:signum-point
   #:*taxicab-directions*
   #:*all-directions-including-diagonals*
   #:make-asset-filename
   #:read-json-file
   #:remove-suffix
   #:make-keyword-list))

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

(defun truncate-point (point &optional (divisor 1))
  (point (truncate (x point) divisor)
         (truncate (y point) divisor)))

(defun signum-point (point)
  (point (signum (x point))
         (signum (y point))))

(defvar *taxicab-directions*
  '(#C(1 0) #C(0 1) #C(-1 0) #C(0 -1)))

(defvar *all-directions-including-diagonals*
  '(#C(1 0) #C(0 1) #C(-1 0) #C(0 -1)
    #C(-1 -1) #C(1 1) #C(-1 1) #C(1 -1)))

(defun make-asset-filename (game name type)
  (merge-pathnames
   (make-pathname :name (string-downcase name)
                  :type type)
   (assets-directory game)))

(defun read-json-file (filename)
  (let ((com.gigamonkeys.json:*object-type* :hash-table))
    (com.gigamonkeys.json:parse-json
     (alexandria:read-file-into-string filename))))

(defun remove-suffix (sequence suffix)
  (if (alexandria:ends-with-subseq suffix sequence)
      (subseq sequence 0 (- (length sequence) (length suffix)))
      sequence))

(defun make-keyword-list (string)
  (let ((tokens
          (split-sequence:split-sequence #\Space string :remove-empty-subseqs t)))
    (mapcar (lambda (token)
              (alexandria:make-keyword (string-upcase token)))
            tokens)))
