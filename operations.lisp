(defpackage "COLOURS.OPERATIONS"
  (:use "CL" "COLOURS.SPACES")
  (:local-nicknames ("C.C" "COLOURS.CLASS"))
  (:export "MIX" "ADD" "DIST"))
(in-package "COLOURS.OPERATIONS")

(defun add/2 (c₁ c₂)
  (c.c:with-colours (((XYZ c₁) x₁ y₁ z₁)
                     ((XYZ c₂) x₂ y₂ z₂))
    (c.c:make-colour 'XYZ (+ x₁ x₂) (+ y₁ y₂) (+ z₁ z₂))))

(defun add (&rest colours)
  (let ((x 0) (y 0) (z 0))
    (dolist (colour colours)
      (c.c:with-colour ((XYZ colour) xx yy zz)
        (incf x xx)
        (incf y yy)
        (incf z zz)))
    (c.c:make-colour 'XYZ x y z)))

(defun mix/2 (c₁ c₂)
  (c.c:with-colours (((XYZ c₁) x₁ y₁ z₁)
                     ((XYZ c₂) x₂ y₂ z₂))
    (c.c:make-colour 'XYZ (/ (+ x₁ x₂) 2) (/ (+ y₁ y₂) 2) (/ (+ z₁ z₂) 2))))

(defun mix (colour &rest colours)
  (let ((x 0) (y 0) (z 0) (n 0))
    (dolist (colour (cons colour colours))
      (c.c:with-colour ((XYZ colour) xx yy zz)
        (incf x xx) (incf y yy) (incf z zz) (incf n 1)))
    (c.c:make-colour 'XYZ (/ x n) (/ y n) (/ z n))))

(defun dist (c₁ c₂)
  (c.c:with-colours (((CIELAB c₁) L₁ a₁ b₁)
                     ((CIELAB c₂) L₂ a₂ b₂))
    (let ((ΔL² (expt (- L₁ L₂) 2))
          (Δa² (expt (- a₁ a₂) 2))
          (Δb² (expt (- b₁ b₂) 2)))
      (sqrt (+ ΔL² Δa² Δb²)))))

(defun grayscale (c white)
  (c.c:with-colours (((XYZ c) x y z)
                     ((XYZ white) wx wy wz))
    (declare (ignore x z))
    (c.c:make-colour 'XYZ (alexandria:lerp y 0 wx) (alexandria:lerp y 0 wy) (alexandria:lerp y 0 wz))))

;;; FIXME: the choice of CIELAB here is somewhat arbitrary; doing
;;; linear interpolation in some transformation of CIELAB (or,
;;; equivalently, curve interpolation in CIELAB) might get better
;;; results.  Find a way to allow the user to specify the colour
;;; space?
(defun palette (n c₁ c₂)
  (c.c:with-colours (((CIELAB c₁) L₁ a₁ b₁)
                     ((CIELAB c₂) L₂ a₂ b₂))
    (loop for v from 0 to 1 by (/ 1 n)
          for L = (alexandria:lerp v L₁ L₂)
          for a = (alexandria:lerp v a₁ a₂)
          for b = (alexandria:lerp v b₁ b₂)
          collect (c.c:make-colour 'CIELAB L a b))))
