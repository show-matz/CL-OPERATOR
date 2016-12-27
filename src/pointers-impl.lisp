#|
#|ASD|#			   (:file "pointers-impl" :depends-on ("pointers" "operators"))
#|EXPORT|#				;pointers-impl.lisp
 |#


(in-package :cl-operator)


(defmacro __vec-ptr-chk-idx (buf-sym idx-sym)
  (check-type buf-sym symbol)
  (check-type idx-sym symbol)
  `(unless (and (<= 0 ,idx-sym)
				(<    ,idx-sym (length ,buf-sym)))
	 (error 'undefined-behavior :what (format nil "index ~A is out of range." ,idx-sym))))


;-------------------------------------------------------------------------------
;
; implementation of const-fixed-pointer
;
;-------------------------------------------------------------------------------
(defmethod operator_clone ((ptr const-fixed-pointer))
  (make-instance 'const-fixed-pointer
				 :closure (fixed-ptr-closure ptr)))

(defmethod operator_* ((ptr const-fixed-pointer))
  (funcall (fixed-ptr-closure ptr)))

(defmethod (setf operator_*) (new-val (ptr const-fixed-pointer))
  (error 'setf-to-const :what "setf to (operator_* const-fixed-pointer)."))

(defmethod operator_= ((ptr1 const-fixed-pointer) (ptr2 const-fixed-pointer))
  (__error-when-const-removing-assign ptr1 fixed-pointer
									  ptr2 const-fixed-pointer)
  (setf (fixed-ptr-closure ptr1) (fixed-ptr-closure ptr2))
  ptr1)

;-------------------------------------------------------------------------------
;
; implementation of fixed-pointer
;
;-------------------------------------------------------------------------------
(defmethod operator_clone ((ptr fixed-pointer))
  (make-instance 'fixed-pointer
				 :closure (fixed-ptr-closure ptr)))

(defmethod operator_cast ((ptr fixed-pointer)
						  (typename (eql 'const-fixed-pointer)))
  (__check-exact-type-of-cast ptr 'fixed-pointer 'const-fixed-pointer)
  (make-instance 'const-fixed-pointer :closure (fixed-ptr-closure ptr)))

(defmethod (setf operator_*) (new-val (ptr fixed-pointer))
  (funcall (fixed-ptr-closure ptr) new-val))


;;------------------------------------------------------------------------------
;;
;; implementation of const-vector-pointer
;;
;;------------------------------------------------------------------------------
(locally (declare (optimize speed))
  (defmethod operator_= ((ptr1 const-vector-pointer)
						 (ptr2 const-vector-pointer))
	(__error-when-const-removing-assign ptr1 vector-pointer
										ptr2 const-vector-pointer)
	(setf (vec-ptr-buffer ptr1) (vec-ptr-buffer ptr2))
	(setf (vec-ptr-index  ptr1) (vec-ptr-index  ptr2))
	ptr1))

(locally (declare (optimize speed))
  (defmethod operator_clone ((ptr const-vector-pointer))
	(make-instance 'const-vector-pointer
				   :buffer (vec-ptr-buffer ptr)
				   :index  (vec-ptr-index  ptr))))

(locally (declare (optimize speed))
  (defmethod operator_* ((ptr const-vector-pointer))
	(let ((buf   (vec-ptr-buffer ptr))
		  (index (vec-ptr-index  ptr)))
	  (declare (type vector buf))
	  (declare (type fixnum index))
	  (__vec-ptr-chk-idx buf index)
	  (aref buf index))))

(locally (declare (optimize speed))
  (defmethod (setf operator_*) (new-val (ptr const-vector-pointer))
	(error 'setf-to-const :what "setf to (operator_* const-vector-pointer).")))

(locally (declare (optimize speed))
  (defmethod operator_++ ((ptr const-vector-pointer))
	(incf (vec-ptr-index ptr))
	ptr))

(locally (declare (optimize speed))
  (defmethod operator_-- ((ptr const-vector-pointer))
	(decf (vec-ptr-index ptr))
	ptr))

(locally (declare (optimize speed))
  (defmethod operator_== ((ptr1 const-vector-pointer) (ptr2 const-vector-pointer))
	(and (eq (the vector (vec-ptr-buffer ptr1))
			 (the vector (vec-ptr-buffer ptr2)))
		 (=  (the fixnum (vec-ptr-index  ptr1))
			 (the fixnum (vec-ptr-index  ptr2))))))

(locally (declare (optimize speed))
  (defmethod operator_/= ((ptr1 const-vector-pointer) (ptr2 const-vector-pointer))
	(or (not (eq (the vector (vec-ptr-buffer ptr1))
				 (the vector (vec-ptr-buffer ptr2))))
		(/=      (the fixnum (vec-ptr-index  ptr1))
				 (the fixnum (vec-ptr-index  ptr2))))))

(locally (declare (optimize speed))
  (defmethod operator_< ((ptr1 const-vector-pointer) (ptr2 const-vector-pointer))
	(and (eq (the vector (vec-ptr-buffer ptr1))
			 (the vector (vec-ptr-buffer ptr2)))
		 (<  (the fixnum (vec-ptr-index  ptr1))
			 (the fixnum (vec-ptr-index  ptr2))))))

(locally (declare (optimize speed))
  (defmethod operator_<= ((ptr1 const-vector-pointer) (ptr2 const-vector-pointer))
	(and (eq (the vector (vec-ptr-buffer ptr1))
			 (the vector (vec-ptr-buffer ptr2)))
		 (<= (the fixnum (vec-ptr-index  ptr1))
			 (the fixnum (vec-ptr-index  ptr2))))))

(locally (declare (optimize speed))
  (defmethod operator_> ((ptr1 const-vector-pointer) (ptr2 const-vector-pointer))
	(and (eq (the vector (vec-ptr-buffer ptr1))
			 (the vector (vec-ptr-buffer ptr2)))
		 (>  (the fixnum (vec-ptr-index  ptr1))
			 (the fixnum (vec-ptr-index  ptr2))))))

(locally (declare (optimize speed))
  (defmethod operator_>= ((ptr1 const-vector-pointer) (ptr2 const-vector-pointer))
	(and (eq (the vector (vec-ptr-buffer ptr1))
			 (the vector (vec-ptr-buffer ptr2)))
		 (>= (the fixnum (vec-ptr-index  ptr1))
			 (the fixnum (vec-ptr-index  ptr2))))))

(locally (declare (optimize speed))
  (defmethod operator_+ ((ptr const-vector-pointer) (n integer))
	(make-instance 'const-vector-pointer
				   :buffer (vec-ptr-buffer ptr)
				   :index  (+ n (vec-ptr-index ptr)))))

(locally (declare (optimize speed))
  (defmethod operator_+= ((ptr const-vector-pointer) (n integer))
	(incf (vec-ptr-index ptr) n)
	ptr))

(locally (declare (optimize speed))
  (defmethod operator_- ((ptr const-vector-pointer) (n integer))
	(make-instance 'const-vector-pointer
				   :buffer (vec-ptr-buffer ptr)
				   :index  (- (vec-ptr-index ptr) n))))

(locally (declare (optimize speed))
  (defmethod operator_- ((ptr1 const-vector-pointer) (ptr2 const-vector-pointer))
	(unless (eq (the vector (vec-ptr-buffer ptr1))
				(the vector (vec-ptr-buffer ptr2)))
	  (error 'undefined-behavior :what "operator_- : invalid pointer pair."))
	(- (the fixnum (vec-ptr-index ptr1))
	   (the fixnum (vec-ptr-index ptr2)))))

(locally (declare (optimize speed))
  (defmethod operator_-= ((ptr const-vector-pointer) (n integer))
	(decf (vec-ptr-index ptr) n)
	ptr))

(locally (declare (optimize speed))
  (defmethod operator_[] ((ptr const-vector-pointer) (idx integer))
	(let ((buf   (vec-ptr-buffer ptr))
		  (index (+ idx (vec-ptr-index ptr))))
	  (declare (type vector buf))
	  (declare (type fixnum index))
	  (__vec-ptr-chk-idx buf index)
	  (aref buf index))))

(locally (declare (optimize speed))
  (defmethod (setf operator_[]) (new-val (ptr const-vector-pointer) (idx integer))
	(error 'setf-to-const :what "setf to (operator_[] const-vector-pointer).")))


;;------------------------------------------------------------------------------
;;
;; implementation of vector-pointer
;;
;;------------------------------------------------------------------------------
(locally (declare (optimize speed))
  (defmethod operator_clone ((ptr vector-pointer))
	(make-instance 'vector-pointer
				   :buffer (vec-ptr-buffer ptr)
				   :index  (vec-ptr-index  ptr))))

(locally (declare (optimize speed))
  (defmethod operator_cast ((ptr vector-pointer)
							(typename (eql 'const-vector-pointer)))
	(__check-exact-type-of-cast ptr 'vector-pointer 'const-vector-pointer)
	(make-instance 'const-vector-pointer
				   :buffer (vec-ptr-buffer ptr)
				   :index  (vec-ptr-index  ptr))))

(locally (declare (optimize speed))
  (defmethod (setf operator_*) (new-val (ptr vector-pointer))
	(let ((buf   (vec-ptr-buffer ptr))
		  (index (vec-ptr-index  ptr)))
	  (declare (type vector buf))
	  (declare (type fixnum index))
	  (__vec-ptr-chk-idx buf index)
	  (_= (aref buf index) new-val))))

(locally (declare (optimize speed))
  (defmethod operator_+ ((ptr vector-pointer) (n integer))
	(make-instance 'vector-pointer
				   :buffer (vec-ptr-buffer ptr)
				   :index  (+ n (vec-ptr-index ptr)))))

(locally (declare (optimize speed))
  (defmethod operator_- ((ptr vector-pointer) (n integer))
	(make-instance 'vector-pointer
				   :buffer (vec-ptr-buffer ptr)
				   :index  (- (vec-ptr-index ptr) n))))

(locally (declare (optimize speed))
  (defmethod (setf operator_[]) (new-val (ptr vector-pointer) (idx integer))
	(let ((buf   (vec-ptr-buffer ptr))
		  (index (+ idx (vec-ptr-index ptr))))
	  (declare (type vector buf))
	  (declare (type fixnum index))
	  (__vec-ptr-chk-idx buf index)
	  (_= (aref buf index) new-val))))


;;------------------------------------------------------------------------------
;;
;; implementation for const-reverse-vector-pointer
;;
;;------------------------------------------------------------------------------
(locally (declare (optimize speed))
  (defmethod operator_= ((ptr1 const-reverse-vector-pointer)
						 (ptr2 const-reverse-vector-pointer))
	(__error-when-const-removing-assign ptr1 reverse-vector-pointer
										ptr2 const-reverse-vector-pointer)
	(setf (rev-vec-ptr-buffer ptr1) (rev-vec-ptr-buffer ptr2))
	(setf (rev-vec-ptr-index  ptr1) (rev-vec-ptr-index  ptr2))
	ptr1))

(locally (declare (optimize speed))
  (defmethod operator_clone ((ptr const-reverse-vector-pointer))
	(make-instance 'const-reverse-vector-pointer
				   :buffer (rev-vec-ptr-buffer ptr)
				   :index  (rev-vec-ptr-index  ptr))))

(locally (declare (optimize speed))
  (defmethod operator_* ((ptr const-reverse-vector-pointer))
	(let ((buf   (rev-vec-ptr-buffer ptr))
		  (index (rev-vec-ptr-index  ptr)))
	  (declare (type vector buf))
	  (declare (type fixnum index))
	  (__vec-ptr-chk-idx buf index)
	  (aref buf index))))

(locally (declare (optimize speed))
  (defmethod (setf operator_*) (new-val (ptr const-reverse-vector-pointer))
	(error 'setf-to-const :what "setf to (operator_* const-reverse-vector-pointer).")))

(locally (declare (optimize speed))
  (defmethod operator_++ ((ptr const-reverse-vector-pointer))
	(decf (rev-vec-ptr-index ptr))
	ptr))

(locally (declare (optimize speed))
  (defmethod operator_-- ((ptr const-reverse-vector-pointer))
	(incf (rev-vec-ptr-index ptr))
	ptr))

(locally (declare (optimize speed))
  (defmethod operator_== ((ptr1 const-reverse-vector-pointer) (ptr2 const-reverse-vector-pointer))
	(and (eq (the vector (rev-vec-ptr-buffer ptr1))
			 (the vector (rev-vec-ptr-buffer ptr2)))
		 (=  (the fixnum (rev-vec-ptr-index  ptr1))
			 (the fixnum (rev-vec-ptr-index  ptr2))))))

(locally (declare (optimize speed))
  (defmethod operator_/= ((ptr1 const-reverse-vector-pointer) (ptr2 const-reverse-vector-pointer))
	(or (not (eq (the vector (rev-vec-ptr-buffer ptr1))
				 (the vector (rev-vec-ptr-buffer ptr2))))
		(/=      (the fixnum (rev-vec-ptr-index  ptr1))
				 (the fixnum (rev-vec-ptr-index  ptr2))))))

(locally (declare (optimize speed))
  (defmethod operator_< ((ptr1 const-reverse-vector-pointer) (ptr2 const-reverse-vector-pointer))
	(and (eq (the vector (rev-vec-ptr-buffer ptr1))
			 (the vector (rev-vec-ptr-buffer ptr2)))
		 (>  (the fixnum (rev-vec-ptr-index  ptr1))
			 (the fixnum (rev-vec-ptr-index  ptr2))))))

(locally (declare (optimize speed))
  (defmethod operator_<= ((ptr1 const-reverse-vector-pointer) (ptr2 const-reverse-vector-pointer))
	(and (eq (the vector (rev-vec-ptr-buffer ptr1))
			 (the vector (rev-vec-ptr-buffer ptr2)))
		 (>= (the fixnum (rev-vec-ptr-index  ptr1))
			 (the fixnum (rev-vec-ptr-index  ptr2))))))

(locally (declare (optimize speed))
  (defmethod operator_> ((ptr1 const-reverse-vector-pointer) (ptr2 const-reverse-vector-pointer))
	(and (eq (the vector (rev-vec-ptr-buffer ptr1))
			 (the vector (rev-vec-ptr-buffer ptr2)))
		 (<  (the fixnum (rev-vec-ptr-index  ptr1))
			 (the fixnum (rev-vec-ptr-index  ptr2))))))

(locally (declare (optimize speed))
  (defmethod operator_>= ((ptr1 const-reverse-vector-pointer) (ptr2 const-reverse-vector-pointer))
	(and (eq (the vector (rev-vec-ptr-buffer ptr1))
			 (the vector (rev-vec-ptr-buffer ptr2)))
		 (<= (the fixnum (rev-vec-ptr-index  ptr1))
			 (the fixnum (rev-vec-ptr-index  ptr2))))))

(locally (declare (optimize speed))
  (defmethod operator_+ ((ptr const-reverse-vector-pointer) (n integer))
	(make-instance 'const-reverse-vector-pointer
				   :buffer (rev-vec-ptr-buffer ptr)
				   :index  (- (rev-vec-ptr-index ptr) n))))

(locally (declare (optimize speed))
  (defmethod operator_+= ((ptr const-reverse-vector-pointer) (n integer))
	(decf (rev-vec-ptr-index ptr) n)
	ptr))

(locally (declare (optimize speed))
  (defmethod operator_- ((ptr const-reverse-vector-pointer) (n integer))
	(make-instance 'const-reverse-vector-pointer
				   :buffer (rev-vec-ptr-buffer ptr)
				   :index  (+ (rev-vec-ptr-index ptr) n))))

(locally (declare (optimize speed))
  (defmethod operator_- ((ptr1 const-reverse-vector-pointer) (ptr2 const-reverse-vector-pointer))
	(unless (eq (the vector (rev-vec-ptr-buffer ptr1))
				(the vector (rev-vec-ptr-buffer ptr2)))
	  (error 'undefined-behavior :what "operator_- : invalid pointer pair."))
	(- (the fixnum (rev-vec-ptr-index ptr2))
	   (the fixnum (rev-vec-ptr-index ptr1)))))

(locally (declare (optimize speed))
  (defmethod operator_-= ((ptr const-reverse-vector-pointer) (n integer))
	(incf (rev-vec-ptr-index ptr) n)
	ptr))

(locally (declare (optimize speed))
  (defmethod operator_[] ((ptr const-reverse-vector-pointer) (idx integer))
	(let ((buf   (rev-vec-ptr-buffer ptr))
		  (index (- (rev-vec-ptr-index ptr) idx)))
	  (declare (type vector buf))
	  (declare (type fixnum index))
	  (__vec-ptr-chk-idx buf index)
	  (aref buf index))))

(locally (declare (optimize speed))
  (defmethod (setf operator_[]) (new-val (ptr const-reverse-vector-pointer) (idx integer))
	(error 'setf-to-const :what "setf to (operator_[] const-reverse-vector-pointer).")))


;;------------------------------------------------------------------------------
;;
;; implementation for reverse-vector-pointer
;;
;;------------------------------------------------------------------------------
(locally (declare (optimize speed))
  (defmethod operator_clone ((ptr reverse-vector-pointer))
	(make-instance 'reverse-vector-pointer
				   :buffer (rev-vec-ptr-buffer ptr)
				   :index  (rev-vec-ptr-index  ptr))))

(locally (declare (optimize speed))
  (defmethod operator_cast ((ptr reverse-vector-pointer)
							(typename (eql 'const-reverse-vector-pointer)))
	(__check-exact-type-of-cast ptr 'reverse-vector-pointer 'const-reverse-vector-pointer)
	(make-instance 'const-reverse-vector-pointer
				   :buffer (rev-vec-ptr-buffer ptr)
				   :index  (rev-vec-ptr-index  ptr))))

(locally (declare (optimize speed))
  (defmethod (setf operator_*) (new-val (ptr reverse-vector-pointer))
	(let ((buf   (rev-vec-ptr-buffer ptr))
		  (index (rev-vec-ptr-index  ptr)))
	  (declare (type vector buf))
	  (declare (type fixnum index))
	  (__vec-ptr-chk-idx buf index)
	  (_= (aref buf index) new-val))))

(locally (declare (optimize speed))
  (defmethod operator_+ ((ptr reverse-vector-pointer) (n integer))
	(make-instance 'reverse-vector-pointer
				   :buffer (rev-vec-ptr-buffer ptr)
				   :index  (- (rev-vec-ptr-index ptr) n))))

(locally (declare (optimize speed))
  (defmethod operator_- ((ptr reverse-vector-pointer) (n integer))
	(make-instance 'reverse-vector-pointer
				   :buffer (rev-vec-ptr-buffer ptr)
				   :index  (+ (rev-vec-ptr-index ptr) n))))

(locally (declare (optimize speed))
  (defmethod (setf operator_[]) (new-val (ptr reverse-vector-pointer) (idx integer))
	(let ((buf   (rev-vec-ptr-buffer ptr))
		  (index (- (rev-vec-ptr-index ptr) idx)))
	  (declare (type vector buf))
	  (declare (type fixnum index))
	  (__vec-ptr-chk-idx buf index)
	  (_= (aref buf index) new-val))))




