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
        (mapcar (lambda (data) (mgl-gnuplot:data data))
                ,datas)))))

(defun line (x)
  "`w' and `b' are 'parameters' of line.
`x' is the argument of line."
  (lambda (w b)
    (+ (* w x) b)))

(defvar *line-xs* '(2.0 1.0 4.0 3.0))
(defvar *line-ys* '(1.8 1.2 4.2 3.3))

(defun plot-line-xs-ys ()
  (let ((data (loop :for x :in *line-xs*
                    :for y :in *line-ys*
                    :collect (list x y))))
    (plot-data `(,data)
               ;;(mgl-gnuplot:command "set style data linespoints")
               (mgl-gnuplot:command "set xrange [0:5]")
               (mgl-gnuplot:command "set yrange [0:5]")
               (mgl-gnuplot:command "set view map"))))
