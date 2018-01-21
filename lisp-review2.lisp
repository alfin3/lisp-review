;;;; Lisp review 2.
;;;; SBCL 1.3.1
;;;; handy foldr, foldl, filter, mapcar examples

(defun foldr (f u lst)
  "folds a list from right to left"
  (if (null lst) u
    (funcall f (car lst) (foldr f u (cdr lst)))))

(defun foldl (f u lst)
 "folds a list from left to right"
  (if (null lst) u
    (foldl f (funcall f (car lst) u) (cdr lst))))
      
(defun sum (lst)
  "adds up the members of a list"
  (foldr (lambda (head tail) (+ head tail)) 0 lst))

(defun filter (p lst)
  "filters a list according to the predicate p"
  (cond
   ((null lst) lst)
   ((funcall p (car lst)) 
    (cons (car lst) (filter p (cdr lst))))
   (t (filter p (cdr lst)))))

(defun equal-sets-p (list1 list2 &key (test #'eql))
  "checks if two lists respresenting two sets are equal"
  (if (not (= (length list1) (length list2))) NIL
    (foldr (lambda (head tail) (and head tail)) t
           (mapcar (lambda (el2) (member el2 list1 :test test))
		   list2))))

(defun copy-ht (ht key-list)
  "copies ht according to a key-list nondestructively,
   if a key does not exist in ht, then it is mapped to NIL"
  (let ((new-ht (make-hash-table :test (hash-table-test ht))))
    (mapcar (lambda (key)
	      (let ((val (gethash key ht)))
		(setf (gethash key new-ht) val)))
	    key-list)
    new-ht))

(defun random-member (lst)
  "returns a random member of a list"
  (nth (random (length lst)) lst))
