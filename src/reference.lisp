#|
#|ASD|#			   (:file "reference"     :depends-on ("package"))
#|EXPORT|#				;reference.lisp
 |#


(in-package :cl-operator)


;;------------------------------------------------------------------------------
;;
;; class reference
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:reference
 |#
(defclass reference ()
  ((accessor :type     cl:function
			 :initform nil
			 :initarg  :accessor
			 :accessor reference-accessor)))

;;------------------------------------------------------------------------------
;;
;; helper class fo move semantics.
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:remove-reference
|#
(defclass remove-reference (reference) ())

;;------------------------------------------------------------------------------
;;
;; macro with-reference
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:with-reference
|#
(defun reference-access (ref)
  (funcall (reference-accessor ref)))

(defun (setf reference-access) (newval ref)
  (funcall (reference-accessor ref) newval)
  newval)

(defmacro with-reference ((&rest symbols) &body bodies)
  (labels ((make-refsym (symbol)
             (onlisp/symb symbol "&"))
           (make-macrolet (symbol refsym)
             `(,symbol (reference-access ,refsym))))
    (let ((refsyms (mapcar #'make-refsym symbols)))
      `(locally (declare (type reference ,@refsyms))
         (symbol-macrolet ,(mapcar #'make-macrolet symbols refsyms)
           ,@bodies)))))

