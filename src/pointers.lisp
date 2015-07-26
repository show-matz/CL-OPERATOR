#|
#|ASD|#			   (:file "pointers"      :depends-on ("cast" "clone"))
#|EXPORT|#				;pointers.lisp
 |#


(in-package :cl-operator)


;;------------------------------------------------------------------------------
;;
;; class pointer
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:pointer
 |#
(defclass pointer () ())


;;------------------------------------------------------------------------------
;;
;; class fixed-pointer
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:fixed-pointer
#|EXPORT|#				:const-fixed-pointer
 |#
(defclass const-fixed-pointer (pointer clonable)
  ((getter :type     cl:function
		   :initform nil
		   :initarg  :getter
		   :accessor fixed-ptr-getter)))

(defclass fixed-pointer (const-fixed-pointer)
  ((setter :type     cl:function
		   :initform nil
		   :initarg  :setter
		   :accessor fixed-ptr-setter)))


;;------------------------------------------------------------------------------
;;
;; class vector-pointer
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:vector-pointer
#|EXPORT|#				:const-vector-pointer
 |#
(defclass const-vector-pointer (pointer clonable)
  ((buffer :type     :vector
		   :initform nil
		   :initarg  :buffer
		   :accessor vec-ptr-buffer)
   (index  :type     :integer
		   :initform 0
		   :initarg  :index
		   :accessor vec-ptr-index)))

(defclass vector-pointer (const-vector-pointer) ())


;;------------------------------------------------------------------------------
;;
;; class reverse-vector-pointer
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:reverse-vector-pointer
#|EXPORT|#				:const-reverse-vector-pointer
 |#
(defclass const-reverse-vector-pointer (pointer clonable)
  ((buffer :type     :vector
		   :initform nil
		   :initarg  :buffer
		   :accessor rev-vec-ptr-buffer)
   (index  :type     :integer
		   :initform 0
		   :initarg  :index
		   :accessor rev-vec-ptr-index)))

(defclass reverse-vector-pointer (const-reverse-vector-pointer) ())

