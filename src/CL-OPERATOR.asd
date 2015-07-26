(defsystem :CL-OPERATOR
  :description "CL-OPERATOR : a utility of operator overloading."
  :version "0.9.0"
  :author "show-matz <show@architect-matsuoka.jpn.org>"
  :depends-on ("cl-overload")
  :components (#|--------------------------------------------------------------------|#
			   (:file "cast"          :depends-on ("package"))
			   (:file "clone"         :depends-on ("package"))
			   (:file "exceptions"    :depends-on ("clone" "operators"))
			   (:file "operators"     :depends-on ("cast" "clone" "pointers"))
			   (:file "package")
			   (:file "pointers-impl" :depends-on ("pointers" "operators"))
			   (:file "pointers"      :depends-on ("cast" "clone"))
			   #|--------------------------------------------------------------------|#))
