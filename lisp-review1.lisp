;;; Lisp review 1.
;;; SBCL 1.3.1
;;; tail recursion, mutual recursion, lists and pointers

(defun transpose (matrix)
 "matrix transpose
  matrix: list of lists representing a matrix row each"
 (if (equal  (cdr matrix) ())
     (mapcar (lambda (x) (list x))(car matrix))
     (mapcar #'cons (car matrix) (transpose (cdr matrix)))))

(defun matrix-multiply (mat-a mat-b)
  "multiplies two matrices of compatible dimensions
   mat-a: 2D-array 
   mat-b: 2D-array
   "
  (let* ((rows (array-dimension mat-a 0))
         (columns (array-dimension mat-b 1))
         (inner-calc-index (array-dimension mat-b 0))
         (new-array (make-array (list rows columns) :initial-element nil)))
    (dotimes (i rows)
      (dotimes (j columns)
	(setf (aref new-array i j)
	      (let ((accum 0))
		(dotimes (calc-index inner-calc-index accum)
		  (setf accum (+ accum (* (aref mat-a i calc-index)
                                          (aref mat-b calc-index j)))))))))
    new-array))

(defun super-reverse (list)
  "tail-recursively reverses the order of the list and all sublists.
   list: a list of any elements and any number of sublists at any depth"
  (labels
      ((srev-helper (base process-list)
	 (cond
	   ((null process-list) base)
	   ((listp (car process-list))
	    (srev-helper (cons (srev-helper () (car process-list)) base)
			 (cdr process-list)))
	   (t (srev-helper (cons (car process-list) base)
			   (cdr process-list))))))
    (srev-helper () list)))

(defun add-position-iter (list &key (number-from 0))
  "increments (by interation) each of the original numbers
   by the position of that number in the list, nondestructively
   list: list of numbers
   number-from: optional argument (default 0)"
  (let ((newlist (copy-list list)))
    (dotimes (i (length list))
      (setf (nth i newlist) (+ number-from i (nth i newlist))))
    newlist))

(defun add-position-recur (list &key (number-from 0))
   "increments (by recursion) each number by the position of that 
   number in the list, nondestructively
   list: list of numbers
   number-from: optional argument (default 0)"
   (if (null list)
       ()
       (cons (+ number-from (car list))
	     (add-position-recur (cdr list)
				 :number-from (+ number-from 1)))))

(defun add-position-map (list &key (number-from 0))
   "increments (by mapcar) each of the original numbers by the position of that 
    number in the list, nondestructively
    list: list of numbers
    number-from: optional argument (default 0)"
   (mapcar (lambda (x)
	     (let ((new-value (+ number-from x)))
	       (setf number-from (+ number-from 1))
	       new-value))
	   list))

(defun super-remove (elt list &key (test #'eql))
  "removes all instances of the object from the list at any nesting depth, 
   nondestructively
   elt: any object
   list: list
   test: binary predicate"
  (cond
    ((null list) ())
    ((funcall test (car list) elt) (super-remove elt (cdr list) :test test))
    ((listp (car list)) (cons (super-remove elt (car list) :test test)
			      (super-remove elt (cdr list) :test test)))
    (t (cons (car list) (super-remove elt (cdr list) :test test)))))

(defun super-delete (elt list &key (test #'eql))
  "destructively deletes instances of elt in the list using
   mutual recursion of the helper functions
   elt: any object
   list: list
   test: binary predicate"
  (progn
    (cond
      ((funcall test (car list) elt)
       (setf list (cdr list)) ;no change to the original pointer though
       (super-delete-helper-r elt list test)
       (super-delete-helper-l elt list test))
      ((funcall test (cdr list) elt)
       (setf (cdr list) ()) 
       (super-delete-helper-l elt list test))
      (t (super-delete-helper-r elt list test)
	 (super-delete-helper-l elt list test)))
    list))

(defun super-delete-helper-r (elt prev test)
  "helper for recursion to the right, assumes (cdr prev) is not an elt"
  (let ((cur (cdr prev)))
    (cond
      ((atom cur) ()) ;not an elt
      ((null cur) ())
      ((funcall test (car cur) elt)
       (setf (cdr prev) (cdr cur))
       (super-delete-helper-r elt prev test))
      ((funcall test (cdr cur) elt)
       (setf (cdr cur) ())
       (super-delete-helper-l elt cur test))
      (t (super-delete-helper-r elt cur test)
	 (super-delete-helper-l elt cur test)))))

(defun super-delete-helper-l (elt prev test)
  "helper for recursion to the left, assumes (car prev) is not an elt"
   (let ((cur (car prev)))
     (cond
      ((atom cur) ()) ;not an elt
      ((null cur) ())
      ((funcall test (car cur) elt)
       (setf (car prev) (cdr cur))
       (super-delete-helper-l elt prev test))
      ((funcall test (cdr cur) elt)
       (setf (cdr cur) ())
       (super-delete-helper-r elt cur test))
      (t (super-delete-helper-r elt cur test)
	 (super-delete-helper-l elt cur test)))))
           
;; EOF - lisp-review1.lisp
