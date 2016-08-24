#|
#|ASD|#			   (:file "methods"       :depends-on ("operators"))
#|EXPORT|#				;methods.lisp
 |#


(in-package :cl-operator)



(locally (declare (optimize speed))
  (defun __conslist-clone (lst)
	(labels ((imp (src top last)
			   (if (null src)
				   top
				   (let ((node (cons nil nil)))
					 (_= (car node) (car src))
					 (setf (cdr last) node)
					 (imp (cdr src) top node)))))
	  (if (null lst)
		  nil
		  (let ((top (cons nil nil)))
			(_= (car top) (car lst))
			(imp (cdr lst) top top))))))


(locally (declare (optimize speed))

  (defmethod operator_clone ((obj cl:string))
	(copy-seq obj))

  (defmethod operator_clone ((obj cl:list))
	(__conslist-clone obj))

  (defmethod operator_clone ((obj cl:vector))
	(declare (type cl:vector obj))
	(let* ((cnt (length obj))
		   (ret (make-array cnt :initial-element nil)))
	  (declare (type fixnum cnt))
	  (declare (type cl:vector ret))
	  (do ((idx 0 (1+ idx)))
		  ((<= cnt idx) ret)
		(declare (type fixnum idx))
		(_= (aref ret idx) (aref obj idx))))))

