(defsystem :colours
  :author "Christophe Rhodes <christophe@rhodes.io>"
  :description "colour and colour-space functions"
  :serial t
  :depends-on (:alexandria)
  :components ((:file "spaces")
               (:file "search")
               (:file "conversions")
               (:file "class")
               (:file "operations")))
