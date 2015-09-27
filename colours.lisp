(defpackage "COLOUR"
  (:use "COMMON-LISP"))
(in-package "COLOUR")

;;; for now, let's just pretend that everything is tricrhomatic light

#| light + light = light|#
#| light + filter = light|#
#| filter + filter = filter|#

(defclass colour ()
  ())
(defclass colour-space ()
  ((primaries)))
(defvar +XYZ+
  (make-instance 'colour-space
                 :primaries '((1 0 0) (0 1 0) (0 0 1))))
(defvar +sRGB+
  (make-instance 'colour-space
                 :primaries '((0.4124 0.3576 0.1805)
                              (0.2126 0.7152 0.0722)
                              (0.0193 0.1192 0.9505))))

(defun submatrix/1 (matrix i)
  (mapcar (lambda (x) (append (subseq x 0 i) (subseq x (1+ i))))
          matrix))

(defun submatrix/2 (matrix i j)
  (mapcar (lambda (x) (append (subseq x 0 i) (subseq x (1+ i))))
          (append (subseq matrix 0 j) (subseq matrix (1+ j)))))

(defun determinant (matrix)
  (if (= (length matrix) 1)
      (caar matrix)
      (loop for x in (car matrix)
           for k upfrom 0
         for i = 1 then (* i -1)
         sum (* i x (determinant (submatrix/1 (cdr matrix) k))))))

(defun transpose (matrix)
  (apply #'mapcar #'list matrix))

(defun m* (m1 m2)
  (let ((m2 (transpose m2)))
    (loop for x1 in m1
       collect (loop for x2 in m2
                  collect (loop for y1 in x1 for y2 in x2
                               sum (* y1 y2))))))

(defun invert (matrix)
  (loop for i from 0 for l in matrix for f = 1 then (* f -1)
     collect (loop for j from 0 for l in matrix for g = 1 then (* g -1)
                collect
                  (* f g (determinant (submatrix/2 matrix i j))))))

    e2
   /
  /   x
 /________e1

x = a e1 + b e2

dot product with z unit vector at right angles to e2:
x.z = a e1.z
a = x.z/e1.z
b = x.w/e2.w

e1.z = cos 30 in this case = 1/2
x = 1,1
e2 = 1/2, sqrt3/2
z = sqrt3/2, -1/2
w = 0,1
a = (sqrt3/2-1/2)/(sqrt3/2) = 1-1/sqrt3
b = 1/(sqrt3/2)



;;; Implements colour conversions between sRGB, CIE XYZ / xyY and CIE
;;; L*a*b* (1976) colour spaces.  A complete implementation would
;;; handle details such as white point (illuminant) and viewing angle,
;;; and would cover all the conversions, possibly seamlessly and
;;; sanely.  This is not (yet) that complete implementation.  (It has
;;; grown when needed to set or verify undergraduate assignments...)

(defun sRGB->XYZ (r g b)
  (flet ((linearize (c)
           (if (< c 0.04045)
               (/ c 12.92)
               (expt (/ (+ c 0.055) 1.055) 2.4))))
    (let ((rl (linearize r))
          (gl (linearize g))
          (bl (linearize b)))
      (values
       (+ (* 0.4124 rl) (* 0.3576 gl) (* 0.1805 bl))
       (+ (* 0.2126 rl) (* 0.7152 gl) (* 0.0722 bl))
       (+ (* 0.0193 rl) (* 0.1192 gl) (* 0.9505 bl))))))

(defun XYZ->sRGB (x y z)
  (declare (type single-float x y z)
           (optimize speed))
  (flet ((gammaize (c)
           (if (< c 0.00304)
               (* 12.92 c)
               (- (* 1.055 (expt c (/ 2.4))) 0.055))))
    (let ((rl (+ (* 3.240625 x) (* -1.537208 y) (* -0.498629 z)))
          (gl (+ (* -0.968931 x) (* 1.875756 y) (* 0.041518 z)))
          (bl (+ (* 0.055710 x) (* -0.204021 y) (* 1.056996 z))))
      (values (gammaize rl) (gammaize gl) (gammaize bl)))))

(defun XYZ->xyY (x y z)
  (let ((sum (+ x y z)))
    (values (/ x sum) (/ y sum) y)))

(defun sRGB->xyY (r g b)
  (multiple-value-call #'XYZ->xyY (sRGB->XYZ r g b)))

(declaim (inline xyY->xyz))
(defun xyY->xyz (x y yy)
  (values (* (/ x y) yy) yy (* (/ (- 1 x y) y) yy)))

(defun xyY->sRGB (x y yy)
  (declare (optimize speed)
           (single-float x y yy))
  (multiple-value-bind (x y z)
       (xyY->xyz x y yy)
    (XYZ->sRGB x y z)))

(defun XYZ->LAB (x y z)
  (flet ((f (c)
           (if (> c (expt (/ 6 29) 3))
               (expt c 1/3)
               (+ (* 1/3 (expt 29/6 2) c) 4/29))))
    ;; E or #+nil D50 illuminant
    (let ((x0 100.00 #+nil 96.42)
          (y0 100.00)
          (z0 100.00 #+nil 82.52))
      (values 
       (- (* 116 (f (/ y y0))) 16)
       (* 500 (- (f (/ x x0)) (f (/ y y0))))
       (* 200 (- (f (/ y y0)) (f (/ z z0))))))))

(defun sRGB->LAB (r g b)
  (apply #'XYZ->LAB (mapcar (lambda (x) (* x 100))
                            (multiple-value-call #'list (sRGB->XYZ r g b)))))

(defun deltaC/sRGB (c1 c2)
  (multiple-value-bind (l1 a1 b1) (apply #'sRGB->LAB c1)
    (multiple-value-bind (l2 a2 b2) (apply #'sRGB->LAB c2)
      (sqrt (+ (expt (- l2 l1) 2) (expt (- a2 a1) 2) (expt (- b2 b1) 2))))))

(defun grayscalize/sRGB (r g b)
  (multiple-value-bind (x y yy) (sRGB->xyY r g b)
    (declare (ignore x y))
    (multiple-value-bind (r g b)
        (xyY->sRGB 0.3127 0.3290 yy)
      (/ (+ r g b) 3))))

(defstruct (colour (:constructor %make-colour))
  (rep (error "missing rep") :type (unsigned-byte 63)))
(defmethod print-object ((c colour) s)
  (let ((x (colour-x c))
        (y (colour-y c))
        (z (colour-z c)))
    (pprint-logical-block (s nil :prefix "#<COLOUR " :suffix ">")
      (format s "X: ~,2F Y: ~,2F Z: ~,2F (x: ~,2F y: ~,2F) ~@:_" x y z (/ x (+ x y z)) (/ y (+ x y z)))
      (apply #'format s "R: ~,2F G: ~,2F B: ~,2F" (multiple-value-list (xyz->srgb x y z)))
      (apply #'format s " (#~2,'0X~2,'0X~2,'0X, ~3:*rgb(~D,~D,~D))" (mapcar (lambda (x) (round (* (min 1 (max 0 x)) 255))) (multiple-value-list (xyz->srgb x y z)))))))
(defun colour-x (colour)
  (let* ((bits (ldb (byte 21 42) (colour-rep colour))))
    (short-to-float bits)))
(defun colour-y (colour)
  (let* ((bits (ldb (byte 21 21) (colour-rep colour))))
    (short-to-float bits)))
(defun colour-z (colour)
  (let* ((bits (ldb (byte 21 0) (colour-rep colour))))
    (short-to-float bits)))

(defun short-floatify (float)
  (multiple-value-bind (mantissa exponent sign) (decode-float float)
    (unless (<= -32 exponent 31)
      (error "exponent"))
    (unless (> sign -1)
      (error "sign"))
    (let* ((m (* mantissa 2))
           (m-1 (- m 1))
           (im (round (* m-1 (expt 2 15))))
           (ie (+ exponent 32)))
      (dpb ie (byte 6 15) im))))
(defun short-to-float (short)
  (let ((mantissa (ldb (byte 15 0) short))
        (exponent (- (ldb (byte 6 15) short) 32)))
    (float (* (1+ (/ mantissa (expt 2 15))) (expt 2 (1- exponent))))))

(defun %make-xyz-colour (x y z)
  (let ((sx (short-floatify (float x)))
        (sy (short-floatify (float y)))
        (sz (short-floatify (float z))))
    (%make-colour :rep (logior (ash sx 42) (ash sy 21) sz))))
(defun %make-rgb-colour (r g b)
  (multiple-value-bind (x y z) (srgb->xyz r g b)
    (%make-xyz-colour x y z)))

(defun colour (x)
  (cond
    ((colour-p x) x)
    ((and (stringp x) (eql 4 (mismatch x "rgb(")))
     (let ((end (position #\) x))
           (spos 4)
           epos rgbs)
       (tagbody
        valid-spos
          (setf epos (position #\, x :start spos :end end))
          (when (null epos) (go last-one))
          (push (parse-integer x :start spos :end epos) rgbs)
          (setf spos (1+ epos))
          (go valid-spos)
        last-one
          (push (parse-integer x :start spos :end end) rgbs)
          (return-from colour (apply #'%make-rgb-colour (nreverse (mapcar (lambda (x) (/ x 255)) rgbs)))))))
    ((and (stringp x) (= (length x) 7)
          (char= (char x 0) #\#))
     (%make-rgb-colour (/ (parse-integer x :start 1 :end 3 :radix 16) 255)
                       (/ (parse-integer x :start 3 :end 5 :radix 16) 255)
                       (/ (parse-integer x :start 5 :end 7 :radix 16) 255)))))

(defmacro with-xyz ((&rest names) c &body body)
  (let ((col (gensym "COL")))
    `(let ((,col ,c))
       (multiple-value-bind (,@names)
           (values (colour-x ,col) (colour-y ,col) (colour-z ,col))
         ,@body))))

(defun colour+ (&rest cs)
  (let ((rx 0) (ry 0) (rz 0))
    (dolist (c cs (%make-xyz-colour rx ry rz))
      (with-xyz (x y z) (colour c)
        (incf rx x)
        (incf ry y)
        (incf rz z)))))
(defun colour+/2 (a b)
  (with-xyz (ax ay az) (colour a)
    (with-xyz (bx by bz) (colour b)
      (%make-xyz-colour (+ ax bx) (+ ay by) (+ az bz)))))

(defun colour* (c n)
  (with-xyz (x y z) (colour c)
    (%make-xyz-colour (* x n) (* y n) (* z n))))
(defun colour/ (c n)
  (with-xyz (x y z) (colour c)
    (%make-xyz-colour (/ x n) (/ y n) (/ z n))))
