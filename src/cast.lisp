#|
#|ASD|#			   (:file "cast"          :depends-on ("package"))
#|EXPORT|#				;cast.lisp
 |#


(in-package :cl-operator)

;;------------------------------------------------------------------------------
;;
;; cast operators
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:operator_cast
#|EXPORT|#				:static-cast
#|EXPORT|#				:dynamic-cast
|#
(defgeneric operator_cast (obj typename))

(defmacro static-cast ((typename) object)
  (check-type typename symbol)
  `(operator_cast ,object ',typename))

(defmacro dynamic-cast ((typename) object)
  (check-type typename symbol)
  (let ((obj (gensym "OBJ")))
	`(let ((,obj ,object))
	   (if (typep ,obj ',typename) ,obj nil))))



