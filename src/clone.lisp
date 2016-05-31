#|
#|ASD|#			   (:file "clone"         :depends-on ("package"))
#|EXPORT|#				;clone.lisp
 |#


(in-package :cl-operator)

;;------------------------------------------------------------------------------
;;
;; generic cloning function declaration
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:clonable
#|EXPORT|#				:operator_clone
#|EXPORT|#				:clone
|#
(defclass clonable () ())

(defgeneric operator_clone (obj)
  (:documentation "Clone object type & it's data."))

;; default implementation of operator_clone is simply returuns it.
(defmethod operator_clone (obj) obj)

(defmacro clone (obj)
  `(operator_clone ,obj))


