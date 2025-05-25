(defpackage :tll.ch1
  (:use :cl)
  (:nicknames :ch1)
  ;;(:export #:)
  )

(in-package :tll.ch1)

(defun line (x)
  "`w' and `b' are 'parameters' (or the parameter set) of line.
`x' is the argument of line.

Finding the parameters (w and b, as _members_ of ϑ (little theta)) from a data set (x) is known as 'learning'.
Where `w' is ϑ_0 and `b' is ϑ_1.

The estimated ϑ can be used to predict a `y' for an `x'"
  (lambda (ϑ)
    ;; predicted 'y'
    (+ (* (first ϑ) x) (second ϑ))))

(defvar *line-xs* '(2.0 1.0 4.0 3.0))
(defvar *line-ys* '(1.8 1.2 4.2 3.3))

(defun %max (ls)
  (reduce #'max ls :initial-value 0))

(defun %plot-line-xs-ys--with-estimated-ϑ (xs ys &optional approx-line-xys)
  (let ((data (loop :for x :in xs
                    :for y :in ys
                    :collect `(,x ,y 1.0 1))))
    (plot-data
     `(,(cons
         "using 1:2:3:4 with points lc rgb variable ps variable pt 7 title 'data set'"
         data)
       ,(if (null approx-line-xys) nil
            (cons
             "using 1:2 with lines lc 4 lw 2 title 'approx line'"
             approx-line-xys)))
               ;;(mgl-gnuplot:command "set style data linespoints")
               ;;(mgl-gnuplot:command "set style data points")
               ;;(mgl-gnuplot:command "set pointsize 2.0")
               (mgl-gnuplot:command (format nil "set xrange [0:~f]" (+ (%max xs) 2)))
               (mgl-gnuplot:command (format nil "set yrange [0:~f]" (+ (%max ys) 2)))
               (mgl-gnuplot:command "set view map"))))

(defun plot-line-xs-ys--with-estimated-ϑ ()
  "Plots the data set with an estimated line which we get ϑ from."
  (%plot-line-xs-ys--with-estimated-θ *line-xs*
                                      *line-ys*
                                      '((0.0 0.0) (4.5 4.5))))

;; -------------------------
;; plotting
;; -------------------------

(setf mgl-gnuplot::*gnuplot-binary* "/opt/homebrew/bin/gnuplot")

(defmacro plot-data (datas &rest commands)
  `(mgl-gnuplot:with-session ()
     (let ((mgl-gnuplot:*command-stream*
             (make-broadcast-stream mgl-gnuplot:*command-stream*
                                    *standard-output*)))
       ,@commands
       (mgl-gnuplot:plot*
        (mapcar (lambda (data) (mgl-gnuplot:data* (cdr data) (car data)))
                ,datas)))))
