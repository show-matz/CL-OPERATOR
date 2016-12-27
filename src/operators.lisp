#|
#|ASD|#			   (:file "operators"     :depends-on ("pointers" "reference"))
#|EXPORT|#				;operators.lisp
 |#


(in-package :cl-operator)

;;------------------------------------------------------------------------------
;;
;; operator declaration
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:operator_++
#|EXPORT|#				:operator_--
#|EXPORT|#				:operator_&
#|EXPORT|#				:operator_const&
#|EXPORT|#				:operator_*
#|EXPORT|#				:operator_[]
#|EXPORT|#				:operator_==
#|EXPORT|#				:operator_/=
#|EXPORT|#				:operator_<
#|EXPORT|#				:operator_<=
#|EXPORT|#				:operator_>
#|EXPORT|#				:operator_>=
#|EXPORT|#				:operator_+
#|EXPORT|#				:operator_-
#|EXPORT|#				:operator_=
#|EXPORT|#				:operator_+=
#|EXPORT|#				:operator_-=
#|EXPORT|#				:operator_!
#|EXPORT|#				:operator_move
|#
(defgeneric operator_++ (x))
(defgeneric operator_-- (x))
(defgeneric operator_& (obj key))
(defgeneric operator_const& (obj key))
(defgeneric operator_* (x))
(defgeneric (setf operator_*) (newval x))
(defgeneric operator_[] (x key))
(defgeneric (setf operator_[]) (newval x key))
(defgeneric operator_== (lhs rhs))
(defgeneric operator_/= (lhs rhs))
(defgeneric operator_<  (lhs rhs))
(defgeneric operator_<= (lhs rhs))
(defgeneric operator_>  (lhs rhs))
(defgeneric operator_>= (lhs rhs))
(defgeneric operator_+  (lhs rhs))
(defgeneric operator_-  (lhs rhs))
(defgeneric operator_=  (lhs rhs))
(defgeneric operator_+= (lhs rhs))
(defgeneric operator_-= (lhs rhs))
(defgeneric operator_!  (x))
(defgeneric operator_move (lhs rhs))


;;------------------------------------------------------------------------------
;;
;; operator macros
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:_++
#|EXPORT|#				:_--
#|EXPORT|#				:_*
#|EXPORT|#				:_[]
#|EXPORT|#				:_&
#|EXPORT|#				:const_&
#|EXPORT|#				:_==
#|EXPORT|#				:_/=
#|EXPORT|#				:_<
#|EXPORT|#				:_<=
#|EXPORT|#				:_>
#|EXPORT|#				:_>=
#|EXPORT|#				:_+
#|EXPORT|#				:_-
#|EXPORT|#				:_=
#|EXPORT|#				:_+=
#|EXPORT|#				:_-=
#|EXPORT|#				:_!
#|EXPORT|#				:ref
#|EXPORT|#				:move
|#
(declare-macro-overload _++ (1 2))
(declare-macro-overload _-- (1 2))

(defmacro-overload _++ (sym)
  (check-type sym symbol)
  `(setf ,sym (operator_++ ,sym)))

(defmacro-overload _++ (sym dummy)
  (declare (ignore dummy))
  (check-type sym symbol)
  `(prog1 (clone ,sym)
	 (setf ,sym (operator_++ ,sym))))

(defmacro-overload _-- (sym)
  (check-type sym symbol)
  `(setf ,sym (operator_-- ,sym)))

(defmacro-overload _-- (sym dummy)
  (declare (ignore dummy))
  (check-type sym symbol)
  `(prog1 (clone ,sym)
	 (setf ,sym (operator_-- ,sym))))

;;(declare-macro-overload _& (1 2))
;;(declare-macro-overload const_& (1 2))
(labels ((__create-fixed-ptr (expr type)
		   (let ((g-tag (gensym "TAG")))
			 (multiple-value-bind (vars forms var set ref) (get-setf-expansion expr)
			   `(let* (,@(mapcar #'cl:list vars forms))
				  (make-instance ',type
								 :closure (lambda (&optional (,@var ',g-tag))
											(if (eq ,@var ',g-tag)
												,ref
												,(__fix-setter set (car var))))))))))
  (defmacro _& (&rest args)
	(let ((cnt (length args)))
	  (cond ((= 1 cnt) (__create-fixed-ptr (car args) 'fixed-pointer))
			((= 2 cnt) `(operator_& ,@args))
			(t (error "can't resolve overload for ~a" '_&)))))

  (defmacro const_& (&rest args)
	(let ((cnt (length args)))
	  (cond ((= 1 cnt) (__create-fixed-ptr (car args) 'const-fixed-pointer))
			((= 2 cnt) `(operator_const& ,@args))
			(t (error "can't resolve overload for ~a" 'const_&))))))



(defmacro _* (x)  `(operator_* ,x))
(defmacro _[] (x key)  `(operator_[] ,x ,key))

(defmacro _== (lhs rhs) `(operator_== ,lhs ,rhs))
(defmacro _/= (lhs rhs) `(operator_/= ,lhs ,rhs))
(defmacro _<  (lhs rhs) `(operator_<  ,lhs ,rhs))
(defmacro _<= (lhs rhs) `(operator_<= ,lhs ,rhs))
(defmacro _>  (lhs rhs) `(operator_>  ,lhs ,rhs))
(defmacro _>= (lhs rhs) `(operator_>= ,lhs ,rhs))
(defmacro _+  (lhs rhs) `(operator_+  ,lhs ,rhs))
(defmacro _-  (lhs rhs) `(operator_-  ,lhs ,rhs))


(defmacro _= (a b)
  (multiple-value-bind (vars forms var set ref) (get-setf-expansion a)
    `(let* (,@(mapcar #'cl:list vars forms))
       (let ((,@var (operator_= ,ref ,b)))
         ,set
         ,@var))))

(defmacro _+= (a b)
  (multiple-value-bind (vars forms var set ref) (get-setf-expansion a)
    `(let* (,@(mapcar #'cl:list vars forms))
       (let ((,@var (operator_+= ,ref ,b)))
         ,set
         ,@var))))

(defmacro _-= (a b)
  (multiple-value-bind (vars forms var set ref) (get-setf-expansion a)
    `(let* (,@(mapcar #'cl:list vars forms))
       (let ((,@var (operator_-= ,ref ,b)))
         ,set
         ,@var))))

(defmacro _! (x)
  `(operator_! ,x))


(labels ((__create-reference (expr type)
		   (let ((g-tag (gensym "TAG"))
				 (g-obj (gensym "OBJ")))
			 (multiple-value-bind (vars forms var set ref) (get-setf-expansion expr)
			   (if (eq type 'reference)
				   `(let* (,@(mapcar #'cl:list vars forms)
						   (,g-obj ,ref))
					  (if (eq (type-of ,g-obj) 'reference)
						  ,g-obj
						  (make-instance 'reference
										 :accessor (lambda (&optional (,@var ',g-tag))
													 (if (eq ,@var ',g-tag)
														 ,ref
														 ,(__fix-setter set (car var)))))))
				   `(let* (,@(mapcar #'cl:list vars forms)
						   (,g-obj ,ref))
					  (typecase ,g-obj
						(remove-reference ,g-obj)
						(reference
						 (make-instance 'remove-reference
										:accessor (reference-accessor ,g-obj)))
						(t
						 (make-instance 'remove-reference
										:accessor (lambda (&optional (,@var ',g-tag))
													(if (eq ,@var ',g-tag)
														,ref
														,(__fix-setter set (car var)))))))))))))
  (defmacro ref (place)
	(__create-reference place 'reference))
  
  (defmacro move (place)
  "
<<signature>>
  (cl-operator:move place)

<<parameters>>
  place  : place to move source.

<<return value>>
  remove-reference to place.
"
	(__create-reference place 'remove-reference)))


;;------------------------------------------------------------------------------
;;
;; default operator implementation.
;;
;;------------------------------------------------------------------------------

;; default
(defmethod operator_++ (x)   (1+ x))
(defmethod operator_-- (x)   (1- x))
(defmethod operator_== (a b) (eql a b))
(defmethod operator_/= (a b) (not (eql a b)))
(defmethod operator_<  (a b) (<  a b))
(defmethod operator_<= (a b) (<= a b))
(defmethod operator_>  (a b) (>  a b))
(defmethod operator_>= (a b) (>= a b))
(defmethod operator_+  (a b) (+ a b))
(defmethod operator_-  (a b) (- a b))
(defmethod operator_=  (a b) b)
(defmethod operator_+= (a b) (+ a b))
(defmethod operator_-= (a b) (- a b))
(defmethod operator_!  (x)   (null x))
(defmethod operator_!  ((x number)) (zerop x))

;; default assign to nil operator for clonable.
;; MEMO : pair ( cons ) is not 'clonable'.
(defmethod operator_= (a (b clonable))
  (clone b))

;; for move semantics
(defmethod operator_= :around (a (b remove-reference))
  (multiple-value-bind (lhs rhs)
	  (operator_move a (funcall (the cl:function (reference-accessor b))))
	(funcall (the cl:function (reference-accessor b)) rhs)
	lhs))

(defmethod operator_move (lhs rhs)
  (if (eq lhs rhs)
	  (values lhs rhs)
	  (values rhs nil)))

;; casting
(defmethod  operator_cast (obj typename)
  (unless (typep obj typename)
	(error 'type-mismatch :what (format nil "Can't cast ~A to ~A." obj typename)))
  obj)

(defmethod operator_cast ((obj reference) typename)
	(operator_cast (funcall (the cl:function (reference-accessor obj))) typename))


;; string
(defmethod operator_== ((a string) (b string)) (string=  a b))
(defmethod operator_/= ((a string) (b string)) (string/= a b))
(defmethod operator_<  ((a string) (b string)) (string<  a b))
(defmethod operator_<= ((a string) (b string)) (string<= a b))
(defmethod operator_>  ((a string) (b string)) (string>  a b))
(defmethod operator_>= ((a string) (b string)) (string>= a b))
(defmethod operator_+  ((a string) (b string)) (concatenate 'string a b))
(defmethod operator_+= ((a string) (b string)) (concatenate 'string a b))

;; character
(defmethod operator_== ((a character) (b character)) (char=  a b))
(defmethod operator_/= ((a character) (b character)) (char/= a b))
(defmethod operator_<  ((a character) (b character)) (char<  a b))
(defmethod operator_<= ((a character) (b character)) (char<= a b))
(defmethod operator_>  ((a character) (b character)) (char>  a b))
(defmethod operator_>= ((a character) (b character)) (char>= a b))
(defmethod operator_+  ((a character) (b integer))   (code-char (+ (char-code a) b)))
(defmethod operator_+  ((a integer)   (b character)) (code-char (+ (char-code b) a)))
(defmethod operator_+= ((a character) (b integer))   (code-char (+ (char-code a) b)))
(defmethod operator_-  ((a character) (b integer))   (code-char (- (char-code a) b)))
(defmethod operator_-  ((a integer)   (b character)) (- a (char-code b)))
(defmethod operator_-= ((a character) (b integer))   (code-char (- (char-code a) b)))


;; cl:vector
(locally (declare (optimize speed))
  (defmethod operator_[] ((arr cl:vector) (idx fixnum))
	(declare (type cl:vector arr))
	(declare (type fixnum idx))
	(aref arr idx))

  (defmethod (setf operator_[]) (newval (arr cl:vector) (idx fixnum))
	(declare (type cl:vector arr))
	(declare (type fixnum idx))
	(setf (aref arr idx) newval)))
	
(defmethod operator_& ((arr cl:vector) (idx fixnum))
  (make-instance 'vector-pointer :buffer arr :index idx))
  
(defmethod operator_const& ((arr cl:vector) (idx fixnum))
  (make-instance 'const-vector-pointer :buffer arr :index idx))


;;------------------------------------------------------------------------------
;;
;; macro with-operators
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:with-operators
|#
(defun __is_++x (name cnt)
  (and (< 2 cnt) (string= "++" name :end2 2)))

(defun __is_--x (name cnt)
  (and (< 2 cnt) (string= "--" name :end2 2)))

(defun __is_x++ (name cnt)
  (and (string/= name "OPERATOR_++")
	   (string/= name "_++")
	   (< 2 cnt) (string= "++" name :start2 (- cnt 2))))

(defun __is_x-- (name cnt)
  (and (string/= name "OPERATOR_--")
	   (string/= name "_--")
	   (< 2 cnt) (string= "--" name :start2 (- cnt 2))))

(defun __is_*x (name cnt)
  (and (< 1 cnt) (char= (char name 0) #\*)))

(defun __is_&x (name cnt)
  (and (< 1 cnt) (char= (char name 0) #\&)))

(defun __is_C&x (name cnt)
  (and (< 7 cnt) (string= "CONST_&" name :end2 7)))

(defun __is_x[n] (name cnt)
  (let ((pos (position #\[ name)))
	(and pos (< 0 pos (- cnt 2)) (char= (char name (1- cnt)) #\]))))

(defun __is_&x[n] (name cnt)
  (and (__is_&x   name cnt)
	   (__is_x[n] (subseq name 1) (1- cnt))))

(defun __is_C&x[n] (name cnt)
  (and (__is_C&x  name cnt)
	   (__is_x[n] (subseq name 7) (- cnt 7))))

(defun __is_!x (name cnt)
  (and (< 1 cnt) (char= (char name 0) #\!)))

(defun __is_@~x (name cnt)    ; means (clone x)
  (and (< 2 cnt) (string= "@~" name :end2 2)))


(defmacro with-operators (&rest body)
  (labels ((integer-check (substr)
			 (handler-case (parse-integer substr)
			   (error () substr)))

		   (operated-symbolp (x)
			 (when (and (symbolp x) (not (keywordp x)))
			   (let* ((name (symbol-name x))
					  (cnt  (length name)))
				 (or (__is_++x    name cnt)
					 (__is_--x    name cnt)
					 (__is_x++    name cnt)
					 (__is_x--    name cnt)
					 (__is_*x     name cnt)
					 (__is_&x     name cnt)
					 (__is_C&x    name cnt)
					 (__is_x[n]   name cnt)
					 (__is_&x[n]  name cnt)
					 (__is_C&x[n] name cnt)
					 (__is_!x     name cnt)
					 (__is_@~x    name cnt)))))

		   (make-symbol-macrolet (sym)
			 (let* ((name (symbol-name sym))
					(cnt  (length name)))
			   (cond
				 ((__is_++x   name cnt) `(,sym (_++   ,(onlisp/symb (subseq name 2)))))
				 ((__is_--x   name cnt) `(,sym (_--   ,(onlisp/symb (subseq name 2)))))
				 ((__is_x++   name cnt) `(,sym (_++   ,(onlisp/symb (subseq name 0 (- cnt 2))) 0)))
				 ((__is_x--   name cnt) `(,sym (_--   ,(onlisp/symb (subseq name 0 (- cnt 2))) 0)))
				 ((__is_*x    name cnt) `(,sym (_*    ,(onlisp/symb (subseq name 1)))))
				 ((__is_!x    name cnt) `(,sym (_!    ,(onlisp/symb (subseq name 1)))))
				 ((__is_@~x   name cnt) `(,sym (clone ,(onlisp/symb (subseq name 2)))))
				 ((__is_&x[n] name cnt)
				  (let* ((pos      (position #\[ name))
						 (operand1 (subseq name 1 pos))
						 (operand2 (integer-check (subseq name (1+ pos) (1- cnt)))))
					`(,sym (operator_& ,(onlisp/symb operand1)
										,(if (integerp operand2)
											 operand2 (onlisp/symb operand2))))))
				 ((__is_C&x[n] name cnt)
				  (let* ((pos      (position #\[ name))
						 (operand1 (subseq name 7 pos))
						 (operand2 (integer-check (subseq name (1+ pos) (1- cnt)))))
					`(,sym (operator_const& ,(onlisp/symb operand1)
											,(if (integerp operand2)
												 operand2 (onlisp/symb operand2))))))
				 ((__is_&x    name cnt) `(,sym (_&      ,(onlisp/symb (subseq name 1)))))
				 ((__is_C&x   name cnt) `(,sym (const_& ,(onlisp/symb (subseq name 7)))))
				 ((__is_x[n]  name cnt)
				  (let* ((pos      (position #\[ name))
						 (operand1 (subseq name 0 pos))
						 (operand2 (integer-check (subseq name (1+ pos) (1- cnt)))))
					`(,sym (operator_[] ,(onlisp/symb operand1)
										,(if (integerp operand2)
											 operand2 (onlisp/symb operand2))))))))))
	(let ((syms (remove-duplicates
				 (remove-if-not #'operated-symbolp (onlisp/flatten body)))))
	  `(symbol-macrolet ,(mapcar #'make-symbol-macrolet syms)
		 ,@body))))


