(defpackage "COLOURS.SEARCH"
  (:use "CL")
  (:export "BFS" "DFS"))
(in-package "COLOURS.SEARCH")

(defun set-difference/preserving-order1 (x y)
  (remove-if (lambda (z) (member z y)) x))

#|

(let ((x '(0 1 2)))
  (flet ((test (y)
           (assert (equal (set-difference/preserving-order1 x (list y))
                          (remove y x)))))
    (test 0)
    (test 1)
    (test 2)
    (test 3))
  (flet ((test (ys)
           (assert (equal (set-difference/preserving-order1 x ys)
                          (remove ys x :test (lambda (yy xx) (member xx yy)))))))
    (test '(0 1))
    (test '(0 2))
    (test '(2 0))
    (test '(1 2 3))
    (test '(3 2 1))))

|#

;;; TODO: test with different edge functions
(defun dfs (linkedp links from to &optional visited)
  (if (funcall linkedp from to)
      (list from to)
      (dolist (link (set-difference/preserving-order1 (funcall links from) visited))
        (let ((x (dfs linkedp links link to (cons from visited))))
          (when x
            (return (cons from x)))))))

(defun bfs (linkedp links from to &optional queue visited)
  (cond
    ((null from))
    ((funcall linkedp (car from) to) (reverse (cons to from)))
    (t
     (flet ((visitedp (x) (member x visited)))
       (let ((nq (append queue (mapcan (lambda (l)
                                         (unless (visitedp l)
                                           (list (cons l from))))
                                       (funcall links (car from))))))
         (bfs linkedp links (car nq) to (cdr nq) (cons (car from) visited)))))))

#|

(flet ((make-ls (g)
         (values
          (lambda (x y) (member y (cdr (assoc x g))))
          (lambda (x) (cdr (assoc x g))))))
  (let ((graph '((0 1) (1 0 2) (2 1 3 4) (3 2 6) (4 2 5) (5 4 6) (6 5))))
    (multiple-value-bind (lp ls) (make-ls graph)
      (let ((b (bfs lp ls (list 0) 5))
            (d (dfs lp ls 0 5)))
        (assert (equal b '(0 1 2 4 5)))
        (assert (equal d '(0 1 2 3 6 5)))))))

|#
