(asdf:defsystem :colours
  :author "Christophe Rhodes <christophe@rhodes.io>"
  :description "colour and colour-space functions"
  :serial t
  :depends-on (:alexandria)
  :around-compile (lambda (f) (let ((o (readtable-case *readtable*)))
                                (unwind-protect
                                    (progn
                                      (setf (readtable-case *readtable*) :invert)
                                      (funcall f))
                                  (setf (readtable-case *readtable*) o))))
  :components ((:file "spaces")
               (:file "search")
               (:file "conversions")
               (:file "class")
               (:file "operations")))
