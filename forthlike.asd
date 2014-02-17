;;;; forthlike.asd

(asdf:defsystem #:forthlike
  :serial t
  :description "Describe forthlike here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria #:anaphora)
  :components ((:file "package")
	       (:file "dqueue")
	       (:file "util")
               (:file "forthlike")))

