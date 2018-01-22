;;;; Lisp review 2.
;;;; SBCL 1.3.1
;;;; handy foldr, foldl, filter

(defun foldr (f u lst)
  "folds a list from right to left"
  (if (null lst) u
    (funcall f (car lst) (foldr f u (cdr lst)))))

(defun foldl (f u lst)
 "folds a list from left to right, tail recursively"
  (if (null lst) u
    (foldl f (funcall f (car lst) u) (cdr lst))))

(defun compose (f1 f2)
  "compose two functions, great for stacking nn layers, see other repos"
  (lambda (x) (funcall f1 (funcall f2 x))))

(defun sum (lst)
  "adds up the members of a list"
  (foldr (lambda (head tail) (+ head tail)) 0 lst))

(defun filterr (p? lst)
  "filters a list according to the predicate p?"
  (foldr (lambda (head tail)
	   (if (funcall p? head) (cons head tail) tail)) '() lst))

(defun filterl (p? lst)
  "filters a list according to the predicate p? tail recursively, 
   resulting in reversed order"
  (foldl (lambda (head tail)
	   (if (funcall p? head) (cons head tail) tail)) '() lst))

(defun equal-sets-p (list1 list2 &key (test #'eql))
  "checks if two lists respresenting two sets are equal"
  (if (not (= (length list1) (length list2))) NIL
    (foldr (lambda (head tail) (and head tail)) t
           (mapcar (lambda (el2) (member el2 list1 :test test))
		   list2))))

(defun sum-filter (p? lst)
  "filter and then sum"
  (sum (filterl p? lst)))
