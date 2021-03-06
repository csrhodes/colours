(defpackage "COLOURS.CONVERSIONS"
  (:use "CL" "SB-MOP" "COLOURS.SPACES")
  (:export "CONVERT")
  (:import-from "SB-INT" "CONSTANT-FORM-VALUE")
  (:import-from "SB-EXT" "DEFGLOBAL")
  (:local-nicknames ("C.S" "COLOURS.SEARCH")))

(in-package "COLOURS.CONVERSIONS")

(defglobal *colour-conversions* nil)

(defun linkedp (x y)
  (getf (getf *colour-conversions* x) y))
(defun links (x)
  (loop for y in (getf *colour-conversions* x) by #'cddr collect y))

(defclass sfunction (funcallable-standard-object)
  ((source :initarg :source :initform (error "must specify source") :reader source))
  (:metaclass funcallable-standard-class))
(defmethod print-object ((o sfunction) stream)
  (print-unreadable-object (o stream :type t :identity nil)
    (let ((*print-level* 2) (*print-length* 3))
      (format stream "~S" (source o)))))

(defmacro slambda (&rest stuff)
  (let ((s `(lambda ,@stuff)))
    `(let ((fin (make-instance 'sfunction :source ',s)))
       (set-funcallable-instance-function fin ,s)
       fin)))

(defmacro %define-colour-conversion ((c1 &rest a1) (c2 &rest a2) forms12 forms21)
  (let ((1->2 (intern (format nil "~S->~S" c1 c2)))
        (2->1 (intern (format nil "~S->~S" c2 c1))))
    `(progn
       (defun ,1->2 (,@a1)
         ,@forms12)
       (defun ,2->1 (,@a2)
         ,@forms21))))

(defmacro define-colour-conversion ((c1 &rest a1) (c2 &rest a2) forms12 forms21)
  `(progn
     (setf (getf (getf *colour-conversions* ',c1) ',c2)
           (slambda (,@a1) ,@forms12))
     (setf (getf (getf *colour-conversions* ',c2) ',c1)
           (slambda (,@a2) ,@forms21))
     '(,c1 ,c2))
  #+nil
  `(progn
     (%define-colour-conversion ,@(cdr whole))
     (setf (getf (getf *colour-conversions* ',(car n1)) ',(car n2))
           ',form12)
     (setf (getf (getf *colour-conversions* ',(car n2)) ',(car n1))
           ',form21)
     '(,(car n1) ,(car n2))))

(define-colour-conversion (sRGB R G B) (sRGB/linear rl gl bl)
  ((declare (type (single-float 0.0f0 1.0f0) R G B)
            (values (single-float 0.0f0 1.0f0) (single-float 0.0f0 1.0f0) (single-float 0.0f0 1.0f0)))
   (flet ((linearize (c)
            (if (< c 0.04045)
                (/ c 12.92)
                (expt (/ (+ c 0.055) 1.055) 2.4))))
     (values (linearize R) (linearize G) (linearize B))))
  ((declare (type single-float rl gl bl)
            (values (single-float 0.0f0 1.0f0) (single-float 0.0f0 1.0f0) (single-float 0.0f0 1.0f0)))
   (flet ((gammaize (c)
            (if (< c 0.00304)
                (* 12.92 c)
                (- (* 1.055 (expt c (/ 2.4))) 0.055)))
          (clamp (x min max)
            (min (max x min) max)))
     (values (gammaize (clamp rl 0.0 1.0))
             (gammaize (clamp gl 0.0 1.0))
             (gammaize (clamp bl 0.0 1.0))))))

(define-colour-conversion (sRGB/linear rl gl bl) (XYZ X Y Z)
  ((declare (type (single-float 0.0f0 1.0f0) rl gl bl)
            (values single-float single-float single-float))
   (values (+ (* 0.4124 rl) (* 0.3576 gl) (* 0.1805 bl))
           (+ (* 0.2126 rl) (* 0.7152 gl) (* 0.0722 bl))
           (+ (* 0.0193 rl) (* 0.1192 gl) (* 0.9505 bl))))
  ((declare (type single-float X Y Z)
            (values single-float single-float single-float))
   (values (+ (* 3.240625 X) (* -1.537208 Y) (* -0.498629 Z))
           (+ (* -0.968931 X) (* 1.875756 Y) (* 0.041518 Z))
           (+ (* 0.055710 X) (* -0.204021 Y) (* 1.056996 Z)))))

(define-colour-conversion (XYZ X Y Z) (xyY x y Y)
  ((declare (type single-float X Y Z)
            (values single-float single-float single-float))
   (let ((X+Y+Z (+ X Y Z)))
     (values (/ X X+Y+Z) (/ Y X+Y+Z) Y)))
  ((declare (type single-float x y Y)
            (values single-float single-float single-float))
   (values (/ (* x Y) y) Y (/ (* (- 1 x y) Y) y))))

(define-colour-conversion (XYZ X Y Z) (CIELAB L* a* b*)
  ((declare (type single-float X Y Z)
            (values single-float single-float single-float))
   (flet ((f (x) (if (> x (expt 6/29 3)) (expt x 1/3) (+ (* 1/3 (expt 29/6 2) x) 4/29))))
     (values (- (* 116 (f Y)) 16)
             (* 500 (- (f X) (f Y)))
             (* 200 (- (f Y) (f Z))))))
  ((declare (type single-float L* a* b*)
            (values single-float single-float single-float))
   (flet ((f¯¹ (x) (if (> x 6/29) (expt x 3) (* (- x 4/29) 3 (expt 6/29 2)))))
     (let* ((y (/ (+ L* 16) 116))
            (x (+ y (/ a* 500)))
            (z (- y (/ b* 200))))
       (values (f¯¹ x) (f¯¹ y) (f¯¹ z))))))

(define-colour-conversion (sRGB R G B) (RGB R G B)
  ((declare (type (single-float 0.0f0 1.0f0) R G B)
            (values (integer 0 255) (integer 0 255) (integer 0 255)))
   (values (round (* R 255)) (round (* G 255)) (round (* B 255))))
  ((declare (type (integer 0 255) R G B)
            (values (single-float 0.0f0 1.0f0) (single-float 0.0f0 1.0f0) (single-float 0.0f0 1.0f0)))
   (values (/ R 255.0) (/ G 255.0) (/ B 255.0))))

(define-colour-conversion (RGB R G B) (HSB H S B)
  ((declare (type (integer 0 255) R G B)
            (values (single-float 0f0 #.(float (* 2 pi) 1.0f0)) (single-float 0f0 1f0) (integer 0 255)))
   (let* ((max (max R G B))
          (min (min R G B))
          (h (cond
               ((= max min) 0)
               ((= max R) (float (mod (* (/ pi 3) (/ (- G B) (- max min))) (* 2 pi)) 1f0))
               ((= max G) (+ (/ (* 2 pi) 3) (/ (- B R) (- max min))))
               ((= max B) (+ (/ (* 4 pi) 3) (/ (- R G) (- max min))))
               (t (error "can't happen"))))
          (s (if (= max 0) 0.0 (- 1.0 (/ min max)))))
     (values (float h 1.0) s max)))
  ((declare (type (single-float 0f0 #.(float (* 2 pi) 1.0f0)) H)
            (type (single-float 0f0 1f0) S)
            (type (integer 0 255) B)
            (values (integer 0 255) (integer 0 255) (integer 0 255)))
   (multiple-value-bind (I F)
       (floor (/ (* 3 H) pi))
     (let ((P (round (* B (- 1 S))))
           (Q (round (* B (- 1 (* F S)))))
           (T (round (* B (- 1 (* (- 1 F) S))))))
       (ecase I
         (0 (values B T P))
         (1 (values Q B P))
         (2 (values P B T))
         (3 (values P Q B))
         (4 (values T P B))
         (5 (values B P Q)))))))

(defun convert (from to)
  (setq from (canonical-or-lose from))
  (setq to (canonical-or-lose to))
  (if (eql from to)
      (lambda (a b c) (values a b c))
      (let ((path (c.s:dfs #'linkedp #'links from to)))
        (lambda (a b c)
          (loop
             (let ((from (car path))
                   (to (cadr path)))
               (when (null to)
                 (return (values a b c)))
               (setf (values a b c)
                     (funcall (getf (getf *colour-conversions* from) to)
                              a b c))
               (setf path (cdr path))))))))

(define-compiler-macro convert (&whole whole &environment env from to)
  (unless (and (constantp from env) (constantp to env))
    (return-from convert whole))
  (let* ((from (canonical-or-lose (constant-form-value from env)))
         (to (canonical-or-lose (constant-form-value to env)))
         (path (and (not (eql from to))
                    (c.s:bfs #'linkedp #'links (list from) to))))
    (when (eql from to)
      (return-from convert `(lambda (a b c) (values a b c))))
    (unless path
      (return-from convert whole))
    (loop for (from to) on path
       while to
       for s = (source (getf (getf *colour-conversions* from) to))
       for f = s then `(lambda (a b c) (multiple-value-call ,s (,f a b c)))
       finally (return f))))

