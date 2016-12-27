#|
#|ASD|#			   (:file "swap"          :depends-on ("operators"))
#|EXPORT|#				;swap.lisp
 |#


(in-package :cl-operator)

;;------------------------------------------------------------------------------
;;
;; swap overload-method declaration & macro definition
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:swap
|#
;; 25.2.2, swap:

; returns : nil
(declare-method-overload swap (2) :make-top nil)
(defmacro swap (a b)
  "
<<signature>>
  (cl-operator:swap a b)

<<parameters>>
  a  : place of contents swapped.
  b  : place of contents swapped.

<<return value>>
  nil.
"
  (multiple-value-bind (vars1 forms1 var1 set1 ref1) (get-setf-expansion a)
	(multiple-value-bind (vars2 forms2 var2 set2 ref2) (get-setf-expansion b)
	  `(let* (,@(mapcar #'cl:list vars1 forms1)
			  ,@(mapcar #'cl:list vars2 forms2))
		 (multiple-value-bind (,@var1 ,@var2)
			 (,(make-overload-name 'cl-operator:swap 2) ,ref1 ,ref2)
		   (declare (ignorable ,@var1 ,@var2))
		   ,(__fix-setter set1 nil)
		   ,(__fix-setter set2 nil)
		   nil)))))



;(defmethod-overload swap (a b)
;  (let ((tmp nil))
;	(_= tmp   a)
;	(_=   a   b)
;	(_=   b tmp)
;	(values a   b)))

(locally (declare (optimize speed))
  (defmethod-overload swap (a b)
	(let ((tmp nil))
	  (multiple-value-bind (x y) (operator_move tmp   a) (setf tmp x) (setf   a y))
	  (multiple-value-bind (x y) (operator_move   a   b) (setf   a x) (setf   b y))
	  (multiple-value-bind (x y) (operator_move   b tmp) (setf   b x) (setf tmp y))
	  (values a b))))

(locally (declare (optimize speed))
  (defmethod-overload swap ((arr1 cl:vector) (arr2 cl:vector))
	(declare (type cl:vector arr1 arr2))
	(let ((len1 (length arr1))
		  (len2 (length arr2)))
	  (declare (type fixnum len1 len2))
	  (unless (= len1 len2)
		(error 'type-mismatch :what "Type mismatch in swap of cl:vector."))
	  (do ((idx 0 (1+ idx)))
		  ((= idx len1) nil)
		(declare (type fixnum idx))
		(swap (aref arr1 idx) (aref arr2 idx))))
	(values arr1 arr2)))


(locally (declare (optimize speed))
  (defmethod-overload swap ((str1 cl:string) (str2 cl:string))
	(values str2 str1)))


(locally (declare (optimize speed))
  (labels ((imp (lst1 lst2)
			 (swap (car lst1) (car lst2))
			 (let ((cdr1 (cdr lst1))
				   (cdr2 (cdr lst2)))
			   (if (and cdr1 cdr2)
				   (imp cdr1 cdr2)
				   (if (and (null cdr1) (null cdr2))
					   nil
					   (if (null cdr1)
						   (progn
							 (setf (cdr lst1) cdr2)
							 (setf (cdr lst2) nil))
						   (progn
							 (setf (cdr lst2) cdr1)
							 (setf (cdr lst1) nil))))))))
	(defmethod-overload swap ((lst1 cl:list) (lst2 cl:list))
	  (if (or (null lst1) (null lst2))
		  (values lst2 lst1)
		  (progn
			(imp lst1 lst2)
			(values lst1 lst2))))))
