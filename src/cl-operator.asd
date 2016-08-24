(defsystem :CL-OPERATOR
  :description "CL-OPERATOR : a utility of operator overloading."
  :version "0.9.4"
  :author "show-matz <show@architect-matsuoka.jpn.org>"
  :licence "LLGPL"
  :depends-on ("cl-overload")
  :components (#|-BEGIN COMPONENTS---------------------------------------------------|#
			   (:file "cast"          :depends-on ("package"))
			   (:file "clone"         :depends-on ("package"))
			   (:file "exceptions"    :depends-on ("clone" "operators"))
			   (:file "methods"       :depends-on ("operators"))
			   (:file "operators"     :depends-on ("pointers" "reference"))
			   (:file "package")
			   (:file "pointers"      :depends-on ("cast" "clone"))
			   (:file "pointers-impl" :depends-on ("pointers" "operators"))
			   (:file "reference"     :depends-on ("package"))
			   (:file "swap"          :depends-on ("operators"))
			   #|-END COMPONENTS-----------------------------------------------------|#))
