#|
#|ASD|#			   (:file "package")
 |#

(provide :cl-operator)

(defpackage		:cl-operator
  (:nicknames	:opr)
  (:use			:common-lisp
				:cl-overload)
  (:export		#|-BEGIN EXPORT-------------|#
				;cast.lisp
				:operator_cast
				:static-cast
				:dynamic-cast
				;clone.lisp
				:clonable
				:operator_clone
				:clone
				;exceptions.lisp
				:what
				:exception
				:type-mismatch
				:undefined-behavior
				:setf-to-const
				;operators.lisp
				:operator_++
				:operator_--
				:operator_&
				:operator_const&
				:operator_*
				:operator_[]
				:operator_==
				:operator_/=
				:operator_<
				:operator_<=
				:operator_>
				:operator_>=
				:operator_+
				:operator_-
				:operator_=
				:operator_+=
				:operator_-=
				:operator_!
				:operator_move
				:remove-reference
				:_++
				:_--
				:_*
				:_[]
				:_&
				:const_&
				:_==
				:_/=
				:_<
				:_<=
				:_>
				:_>=
				:_+
				:_-
				:_=
				:_+=
				:_-=
				:_!
				:move
				:with-operators
				;pointers.lisp
				:pointer
				:fixed-pointer
				:const-fixed-pointer
				:vector-pointer
				:const-vector-pointer
				:reverse-vector-pointer
				:const-reverse-vector-pointer
				;pointers-impl.lisp
				;reference.lisp
				:reference
				:ref
				:with-reference
				;swap.lisp
				:swap
				#|-END EXPORT---------------|#))

(in-package :cl-operator)


;;------------------------------------------------------------------------------
;;
;; internal utilities
;;
;;------------------------------------------------------------------------------
(defun onlisp/mkstr (&rest args)
  (with-output-to-string (s)
	(dolist (a args) (princ a s))))

(defun onlisp/symb (&rest args)
  (values (intern (apply #'onlisp/mkstr args))))

(defun onlisp/flatten (x &optional (acc nil))
  (cond ((null x) acc)
		((atom x) (cons x acc))
		(t (onlisp/flatten (car x) (onlisp/flatten (cdr x) acc)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun __setf-form-p (form)
	(handler-case
		(destructuring-bind (_call (_func (_setf sym)) _newval &rest args) form
		  (declare (ignorable sym _newval args))
		  (when (and (eq _call 'cl:funcall)
					 (eq _func 'cl:function)
					 (eq _setf 'cl:setf))
			(cadr form)))
	  (error (c) (declare (ignorable c)) nil)))

  (defun __setter-exist-p (form)
	(handler-case (eval form)
	  (error (c) (declare (ignorable c)) nil))))

(defmacro __error-when-const-removing-assign (itr1 itrtype itr2 const-itrtype)
  `(when (and (eq (type-of ,itr1) ',itrtype)
			  (eq (type-of ,itr2) ',const-itrtype))
	 (error 'type-mismatch :what ,(format nil "~A can't assign to ~A" const-itrtype itrtype))))

(defmacro __check-exact-type-of-cast (obj fromtype desttype)
  `(unless (eq (type-of ,obj) ,fromtype)
	 (error 'type-mismatch :what (format nil "Can't cast ~A to ~A." (type-of ,obj) ,desttype))))
