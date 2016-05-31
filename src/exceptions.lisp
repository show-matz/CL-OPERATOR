#|
#|ASD|#			   (:file "exceptions"    :depends-on ("clone" "operators"))
#|EXPORT|#				;exceptions.lisp
 |#

(in-package :cl-operator)


;;----------------------------------------------------
#|
#|EXPORT|#				:what
 |#
(defun what (err)
  (check-type err condition)
  (let ((*print-escape* nil))
	(with-output-to-string (stream)
	  (print-object err stream))))

;;----------------------------------------------------
#|
#|EXPORT|#				:exception
 |#
(define-condition exception (cl:error)
  ((what :initarg  :what
		 :accessor exception-what))
  (:report (lambda (condition stream)
             (write-string (exception-what condition) stream))))

(defmethod operator_clone ((obj exception))
  (make-condition (type-of obj) :what (what obj)))

(defmethod operator_= ((lhs (eql nil)) (rhs exception))
  (make-condition (type-of rhs) :what (what rhs)))

(defmethod operator_= ((lhs exception) (rhs exception))
  (unless (eq (type-of lhs) (type-of rhs))
	(error 'type-mismatch :what "Type mismatch in assignment of exceptions."))
  (setf (exception-what lhs) (exception-what rhs))
  lhs)



;;----------------------------------------------------
;; specific errors
;;----------------------------------------------------
#|
#|EXPORT|#				:type-mismatch
#|EXPORT|#				:undefined-behavior
#|EXPORT|#				:setf-to-const
 |#
(define-condition type-mismatch      (exception) ())
(define-condition undefined-behavior (exception) ())
(define-condition setf-to-const      (exception) ())

