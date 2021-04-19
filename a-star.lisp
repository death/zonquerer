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

(defpackage #:zonquerer/a-star
  (:use #:cl)
  (:import-from
   #:pileup)
  (:import-from
   #:fset)
  (:export
   #:a-star))

(in-package #:zonquerer/a-star)

(defvar *equiv-p*)

(defstruct wrapper value)

(defmethod fset:compare ((x wrapper) (y wrapper))
  (if (funcall *equiv-p*
               (wrapper-value x)
               (wrapper-value y))
      :equal
      :unequal))

(defun path-to (node came-from)
  (macrolet ((wrap (x) `(if *equiv-p* (make-wrapper :value ,x) ,x))
             (unwrap (x) `(if *equiv-p* (wrapper-value ,x) ,x)))
    (let ((path (list node)))
      (loop while (fset:lookup came-from (wrap node))
            do (setf node (fset:lookup came-from (wrap node))
                     path (cons node path))
            finally (return (nreverse path))))))

(defun a-star (start &key goal-state-p
                          (failure-p (constantly nil))
                          heuristic
                          expand
                          equiv-p
                          (max-cost most-positive-fixnum))
  "Search for a goal state using the A* algorithm.

If a goal state is found, return the path from the initial state to
the goal state, reversed, as a primary value, and the cost of taking
this path as a secondary value.

START is the initial state.

GOAL-STATE-P is a predicate that accepts a state and returns true if
it is a goal state and false otherwise.

FAILURE-P is an optional predicate that takes a state and returns true
if it should not be visited, and false otherwise.

HEURISTIC is a function taking a state and returning an estimated cost
for the path between it and the goal state.  It may underestimate, but
never overestimate.

EXPAND is a function taking a state and returning a list of 2-element
lists (edge-cost next-state).

EQUIV-P is an optional predicate taking two states and returning true
if they are to be considered equivalent, and false otherwise.  If it
is not supplied, states are considered equivalent if and only if they
are equal under FSET:COMPARE."
  (assert goal-state-p (goal-state-p) "Need goal state predicate.")
  (assert heuristic (heuristic) "Need heuristic function.")
  (assert expand (expand) "Need expansion function.")
  (let ((*equiv-p* equiv-p)
        (counter 0)
        (closed (fset:empty-set))
        (queue (pileup:make-heap #'< :key #'first))
        (best-cost-to (fset:empty-map))
        (came-from (fset:empty-map)))
    (macrolet ((wrap (x) `(if equiv-p (make-wrapper :value ,x) ,x))
               (unwrap (x) `(if equiv-p (wrapper-value ,x) ,x)))
      (pileup:heap-insert (list (funcall heuristic start) (incf counter) start) queue)
      (setf best-cost-to (fset:with best-cost-to (wrap start) 0))
      (loop until (pileup:heap-empty-p queue)
            do (let ((node (third (pileup:heap-pop queue))))
                 (when (funcall goal-state-p node)
                   (return-from a-star
                     (values (path-to node came-from)
                             (fset:lookup best-cost-to (wrap node)))))
                 (setf closed (fset:with closed (wrap node)))
                 (loop for (edge-cost next-node) in (funcall expand node)
                       do (cond ((fset:lookup closed (wrap next-node)))
                                ((funcall failure-p node))
                                (t
                                 (let ((next-cost (+ (fset:lookup best-cost-to (wrap node)) edge-cost)))
                                   (when (< next-cost (or (fset:lookup best-cost-to (wrap next-node))
                                                          max-cost))
                                     (setf came-from (fset:with came-from (wrap next-node) node))
                                     (setf best-cost-to (fset:with best-cost-to (wrap next-node) next-cost))
                                     (pileup:heap-insert (list (+ next-cost (funcall heuristic next-node))
                                                               (incf counter)
                                                               next-node)
                                                         queue)))))))))))
