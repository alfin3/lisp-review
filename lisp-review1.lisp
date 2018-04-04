;;;; Lisp review 1.
;;;; SBCL 1.3.1
;;;; tail recursion, mutual recursion, lists and pointers

(defun transpose (matrix)
 "matrix transpose
  matrix: list of lists representing a matrix row each"
 (if (equal  (cdr matrix) '())
     (mapcar (lambda (x) (list x))(car matrix))
     (mapcar #'cons (car matrix) (transpose (cdr matrix)))))

(defun matrix-multiply (mat-a mat-b)
  "multiplies two matrices of compatible dimensions
   mat-a: 2D-array 
   mat-b: 2D-array"
  (let* ((rows (array-dimension mat-a 0))
         (columns (array-dimension mat-b 1))
         (inner-calc-index (array-dimension mat-b 0))
         (new-array (make-array (list rows columns) :initial-element NIL)))
    (dotimes (i rows)
      (dotimes (j columns)
	(setf (aref new-array i j)
	      (let ((accum 0))
		(dotimes (calc-index inner-calc-index accum)
		  (setf accum (+ accum (* (aref mat-a i calc-index)
                                          (aref mat-b calc-index j)))))))))
    new-array))

(defun super-reverse (lst)
  "tail-recursively reverses the order of the list and all sublists.
   lst: a list of any elements and any number of sublists at any depth"
  (labels
      ((srev-helper (base process-list)
	 (cond
	   ((null process-list) base)
	   ((listp (car process-list))
	    (srev-helper (cons (srev-helper '() (car process-list)) base)
			 (cdr process-list)))
	   (t (srev-helper (cons (car process-list) base)
			   (cdr process-list))))))
    (srev-helper '() lst)))

(defun add-position-iter (lst &key (number-from 0))
  "increments (by interation) each of the original numbers
   by the position of that number in the list, nondestructively
   lst: list of numbers
   number-from: optional argument (default 0)"
  (let ((new-list (copy-list lst)))
    (dotimes (i (length lst))
      (setf (nth i new-list) (+ number-from i (nth i new-list))))
    new-list))

(defun add-position-recur (lst &key (number-from 0))
   "increments (by recursion) each number by the position of that 
   number in the list, nondestructively
   lst: list of numbers
   number-from: optional argument (default 0)"
   (if (null lst)
       '()
       (cons (+ number-from (car lst))
	     (add-position-recur (cdr lst)
				 :number-from (+ number-from 1)))))

(defun add-position-map (lst &key (number-from 0))
   "increments (by mapcar) each of the original numbers by the position of that 
    number in the list, nondestructively
    lst: list of numbers
    number-from: optional argument (default 0)"
   (mapcar (lambda (x)
	     (let ((new-value (+ number-from x)))
	       (setf number-from (+ number-from 1))
	       new-value))
	   lst))

(defun super-remove (el lst &key (test #'eql))
  "removes all instances of the object from the list at any nesting depth, 
   nondestructively
   el: any object
   lst: list
   test: binary predicate"
  (cond
    ((null lst) '())
    ((funcall test (car lst) el) (super-remove el (cdr lst) :test test))
    ((listp (car lst)) (cons (super-remove el (car lst) :test test)
			     (super-remove el (cdr lst) :test test)))
    (t (cons (car lst) (super-remove el (cdr lst) :test test)))))

(defun super-delete (el lst &key (test #'eql))
  "destructively deletes instances of elt in the list using
   mutual recursion of the helper functions
   el: any object
   lst: list
   test: binary predicate"
  (progn
    ;does not change the original pointer though
    (setf lst (filter-first-non-el el lst test))
    (cond
      ((null lst) lst)
      ((funcall test (cdr lst) el)
       (setf (cdr lst) '())
       (super-delete-helper-l el lst test))
      (t (super-delete-helper-r el lst test)
	 (super-delete-helper-l el lst test)))
    lst))

(defun super-delete-helper-r (el prev test)
  "helper for recursion to the right, assumes (cdr prev) is not an el"
  (let ((cur (cdr prev)))
    (cond
      ((atom cur) '())
      ((null cur) '())
      ((funcall test (car cur) el)
       (setf (cdr prev) (cdr cur))
       (super-delete-helper-r el prev test))
      ((funcall test (cdr cur) el)
       (setf (cdr cur) '())
       (super-delete-helper-l el cur test))
      (t (super-delete-helper-r el cur test)
	 (super-delete-helper-l el cur test)))))

(defun super-delete-helper-l (el prev test)
  "helper for recursion to the left, assumes (car prev) is not an el"
   (let ((cur (car prev)))
     (cond
      ((atom cur) '())
      ((null cur) '())
      ((funcall test (car cur) el)
       (setf (car prev) (cdr cur))
       (super-delete-helper-l el prev test))
      ((funcall test (cdr cur) el)
       (setf (cdr cur) '())
       (super-delete-helper-l el cur test))
      (t (super-delete-helper-r el cur test)
	 (super-delete-helper-l el cur test)))))

(defun filter-first-non-el (el lst test)
  "filter the list until the first non-el found"
  (cond
    ((null lst) '()) ;if el and lst are both '()
    ((funcall test (car lst) el)
     (filter-first-non-el el (cdr lst) test))
    (t lst)))
      
;; EOF
