(defpackage "COLOURS.SPACES"
  (:use "CL")
  (:export "CANONICAL" "CANONICAL-OR-LOSE" "DEFINE-COLOUR-SPACE"
           "RGB" "rgb"
           "HSB" "hsb"
           "XYZ" "xyz"
           "xyY"
           "sRGB"
           "sRGB/linear"
           "CIELAB" "cielab"))
(in-package "COLOURS.SPACES")

(defvar *canonical-spaces* nil)
(defmacro define-colour-space (canonical aliases &body body &aux doc)
  (when (and body (stringp (car body)))
    (setq doc (car body) body (cdr body)))
  `(progn
     (setq *canonical-spaces* (set-difference *canonical-spaces* ',aliases))
     (remprop ',canonical 'canonical)
     (remprop ',canonical 'doc)
     (pushnew ',canonical *canonical-spaces*)
     ,@(loop for a in aliases collect `(setf (get ',a 'canonical) ',canonical))
     ,@(when doc
         `((setf (get ',canonical 'doc) ,doc)))))

(defun canonical (id)
  (let ((c (or (get id 'canonical) id)))
    (when (member c *canonical-spaces*)
      c)))

(defun canonical-or-lose (id)
  (let ((c (canonical id)))
    (unless c
      (error "not a colour space: ~S" id))
    c))

(defmethod documentation ((o symbol) (doc-type (eql 'space)))
  (let ((c (canonical o)))
    (when c
      (get c 'doc))))

(define-colour-space RGB (rgb)
  "Red-Green-Blue cube, with values in [0,255]³")
(define-colour-space HSB (hsb)
  "Hue-Saturation-Brightness cone, with values:
- Hue ∈ [0,2π)
- Saturation ∈ [0,1]
- Brightness ∈ [0,255]")
(define-colour-space XYZ (xyz)
  "The CIE (1931) Standard Observer colour space.")
(define-colour-space xyY ()
  "Chromaticity-Luminance transformation of the CIE XYZ colour space,
with values:
- x,y ∈ [0,1]² ∧ x+y ≤ 1
- Y ∈ [0,∞) (as CIE XYZ)")
(define-colour-space sRGB ()
  "A standard default colour space for the Internet (IEC 61966-2-1).
Values in [0,1]³ correspond to RGB values in [0,255]³")
(define-colour-space sRGB/linear ()
  "The CIE (1931) Standard Observer colour space, expressed in terms
of the sRGB primaries.")
(define-colour-space CIELAB (cielab))
