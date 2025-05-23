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

(defun plot-line-xs-ys--with-estimated-ϑ ()
  "Plots the data set with an estimated line which we get ϑ from."
  (let ((data (loop :for x :in *line-xs*
                    :for y :in *line-ys*
                    :collect `(,x ,y 1.0 1)))
        (approx-line-data '((0.0 0.0) (4.5 4.5))))
    (plot-data `(,(cons
                   "using 1:2:3:4 with points lc rgb variable ps variable pt 7 title 'data set'"
                   data)
                 ,(cons
                   "using 1:2 with lines lc 4 lw 2 title 'approx line"
                   approx-line-data))
               ;;(mgl-gnuplot:command "set style data linespoints")
               ;;(mgl-gnuplot:command "set style data points")
               ;;(mgl-gnuplot:command "set pointsize 2.0")
               (mgl-gnuplot:command "set xrange [0:5]")
               (mgl-gnuplot:command "set yrange [0:5]")
               (mgl-gnuplot:command "set view map"))))

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
