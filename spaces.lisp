(eval-when (:compile-toplevel :load-toplevel :execute)
  (make-package "COLOURS.SPACES" :use '("CL")))
(in-package "COLOURS.SPACES")
(export (intern "CANONICAL"))

(defvar *canonical-spaces* nil)
(defmacro define-colour-space (canonical &rest aliases)
  `(progn
     (export (cons ',canonical ',aliases))
     (pushnew ',canonical *canonical-spaces*)
     ,@(loop for a in aliases collect `(setf (get ',a 'canonical) ',canonical))))

(defun canonical (id)
  (let ((c (or (get id 'canonical) id)))
    (unless (member c *canonical-spaces*)
      (error "not a colour space: ~S" c))
    c))

(define-colour-space RGB rgb)
(define-colour-space HSB hsb)
(define-colour-space XYZ xyz)
(define-colour-space xyY)
(define-colour-space sRGB)
(define-colour-space CIELAB cielab)
