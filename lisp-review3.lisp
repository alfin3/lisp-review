;;;; Lisp review 3.
;;;; SBCL 1.3.1
;;;; compile-time optimization, based on On Lisp by Graham

;;;; tail recursion and type declaration

(defun triangle-no-tail-rec (n)
  "(time (triangle-no-tail-rec 50000))"
  (if (zerop n)
      0
      (+ n (triangle-no-tail-rec (- n 1)))))

(defun triangle-no-types (n)
  "(time (triangle-no-types 50000))"
  (labels ((tri (c n)
	     (if (zerop n)
		 c
		 (tri (+ n c)
		      (- n 1)))))
    (tri 0 n)))

(defun triangle (n)
  "(time (triangle 50000))"
  (labels ((tri (c n)
	     (declare (type fixnum n c))
	     (if (zerop n)
		 c
		 (tri (the fixnum (+ n c))
		      (the fixnum (- n 1))))))
    (tri 0 n)))











