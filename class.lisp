(defpackage "COLOURS.CLASS"
  (:use "CL" "COLOURS.CONVERSIONS" "COLOURS.SPACES")
  (:export "COLOUR" "MAKE-COLOUR" "WITH-COLOUR" "WITH-COLOURS"))
(in-package "COLOURS.CLASS")

(defclass colour ()
  ((reps :initarg :reps :initform nil)))
(defmethod print-object ((o colour) s)
  (print-unreadable-object (o s :type t)
    (with-slots (reps) o
      (format s "~A" (car reps))
      (format s " [#~{~2,'0X~}]" (multiple-value-list
                                   (apply (convert (caar reps) 'RGB)
                                          (cdar reps)))))))

(defun make-colour (space x y z)
  (make-instance 'colour :reps (list (list space x y z))))

(defmacro with-colour (((space colour) x y z) &body body)
  `(multiple-value-bind (,x ,y ,z)
       (apply (convert (caar (slot-value ,colour 'reps)) ',space)
              (cdar (slot-value ,colour 'reps)))
     ,@body))
(defmacro with-colours (bindings &body body)
  (when (null bindings)
    (return-from with-colours `(locally ,@body)))
  (loop for ((space colour) x y z) in bindings
     for gs = (map-into (make-list 3) #'gensym)
     collect `((,space ,colour) ,@gs) into colours
     append (list x y z) into vars
     append gs into gensyms
     finally (return
               (labels ((build (cs form)
                          (if (null cs)
                              form
                              `(with-colour ,(car cs)
                                 ,(build (cdr cs) form)))))
                 (build colours
                        `(let ,(loop for v in vars
                                  for g in gensyms
                                  collect (list v g))
                           ,@body))))))
