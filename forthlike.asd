;;;; forthlike.asd

(asdf:defsystem #:forthlike
  :serial t
  :description "Describe forthlike here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:split-sequence)
  :components ((:file "package")
	       (:file "util")
	       (:file "dqueue")
               (:file "forthlike")))

