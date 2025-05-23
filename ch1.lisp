(defpackage :tll.ch1
  (:use :cl)
  (:nicknames :ch1)
  ;;(:export #:)
  )

(in-package :tll.ch1)

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

(defun line (x)
  "`w' and `b' are 'parameters' (or the parameter set) of line.
`x' is the argument of line.

Finding the parameters (w and b, as ϑ (little theta)) from a data set (x) is known as 'learning'."
  (lambda (w b)
    ;; predicted 'y'
    (+ (* w x) b)))

(defvar *line-xs* '(2.0 1.0 4.0 3.0))
(defvar *line-ys* '(1.8 1.2 4.2 3.3))

(defun plot-line-xs-ys ()
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
