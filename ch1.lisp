(defpackage :tll.ch1
  (:use :cl :tens)
  (:nicknames :ch1)
  ;;(:export #:)
  )

(in-package :tll.ch1)

(defun line (x)
  "`w' and `b' are 'parameters' (or the parameter set) of line.
`x' is the argument of line.
`x' can either be a scalar for a single point, or a tensor.

Finding the parameters (w and b, as _members_ of ϑ (little theta)) from a data set (x) is known as 'learning'.
Where `w' is ϑ_0 and `b' is ϑ_1.

The estimated ϑ can be used to predict a `y' for an `x'"
  (lambda (ϑ)
    ;; predicted 'y'
    (t+ (t* (first ϑ) x) (second ϑ))))

(defvar *line-xs* #(2.0 1.0 4.0 3.0))
(defvar *line-ys* #(1.8 1.2 4.2 3.3))

(defun quad (x)
  (lambda (ϑ)
    (t+ (t* (first ϑ) (tsquare x))
        (t+ (t* (second ϑ) x)
            (third ϑ)))))

(defvar *quad-xs* #(-1.0 0.0 1.0 2.0 3.0))
(defvar *quad-ys* #(2.55 2.1 4.35 10.2 18.25))

(defun plane (te)
  (lambda (ϑ)
    (t+ (tdot (first ϑ) te) (second ϑ))))

(defvar *plane-xs*
  #(#(1.0 2.05)
    #(1.0 3.0)
    #(2.0 2.0)
    #(2.0 3.91)
    #(3.0 6.13)
    #(4.0 8.09)))
(defvar *plane-ys*
  #(13.99 15.99 18.0 22.4 30.2 37.94))

;; -----------------------------------------------

(defmacro defhyper (name)
  `(defvar ,name nil))

(defmacro with-hypers (bindings &body body)
  "Bind hyperparameter variables within the scope of BODY.

BINDINGS is a list of (variable value) pairs, similar to LET bindings.
This macro is syntactic sugar for LET, specifically designed for setting
hyperparameters in machine learning contexts.

Example:
  (with-hypers ((*revs* 1000) (alpha 0.01))
    (gradient-descent loss-fn params))

This expands to:
  (let ((*revs* 1000) (alpha 0.01))
    (gradient-descent loss-fn params))"
  `(let ,bindings
     ,@body))

;; -----------------------------------------------

(defhyper *revs*)
(defhyper *α*)

(defun l2-loss (xs ys target-fun)
  (lambda (ϑ)
    ;; pred-ys => predicted ys
    (let* ((pred-ys (funcall
                     (funcall target-fun xs)
                     ϑ))
           (squared-loss (tsum
                          (tsquare
                           (t- ys pred-ys)))))
      (format t "Pred-y: ~a, loss: ~a~%" pred-ys squared-loss)
      squared-loss)))

;;(defun gradient-list (f ϑ &optional (h 1e-5))
;;   "Calculate numerical gradients of function f with respect to each parameter in ϑ"
  ;; (loop :for i :from 0 :below (length ϑ)
  ;;       :collect
  ;;       (let ((ϑ-plus (copy-list ϑ))
  ;;             (ϑ-minus (copy-list ϑ)))
  ;;         ;; Perturb the i-th parameter by +h and -h
  ;;         (setf (nth i ϑ-plus) (t+ (nth i ϑ) h))
  ;;         (setf (nth i ϑ-minus) (t- (nth i ϑ) h))
  ;;         ;; Calculate numerical gradient using central difference
  ;;         (/ (- (funcall f ϑ-plus) (funcall f ϑ-minus))
  ;;            (* 2 h)))))

(defun compute-gradients (loss-fn theta &key (epsilon 1e-5))
  "Compute gradients for theta which is always a list containing scalars and/or vectors"
  (loop :for i :from 0 :below (length theta)
        :collect 
        (let ((param (nth i theta)))
          (cond
            ;; Parameter is a scalar
            ((numberp param)
             (let* ((theta-plus (copy-list theta))
                    (theta-minus (copy-list theta)))
               (setf (nth i theta-plus) (+ param epsilon))
               (setf (nth i theta-minus) (- param epsilon))
               (let* ((loss-plus (funcall loss-fn theta-plus))
                      (loss-minus (funcall loss-fn theta-minus)))
                 (/ (- loss-plus loss-minus) (* 2 epsilon)))))
            
            ;; Parameter is a vector
            ((vectorp param)
             (loop :for j :from 0 :below (length param)
                   :collect (let* ((theta-plus (copy-list theta))
                                  (theta-minus (copy-list theta))
                                  (param-plus (copy-seq param))
                                  (param-minus (copy-seq param)))
                             (incf (aref param-plus j) epsilon)
                             (decf (aref param-minus j) epsilon)
                             (setf (nth i theta-plus) param-plus)
                             (setf (nth i theta-minus) param-minus)
                             (let* ((loss-plus (funcall loss-fn theta-plus))
                                    (loss-minus (funcall loss-fn theta-minus)))
                               (/ (- loss-plus loss-minus) (* 2 epsilon))))
                     :into gradients
                   :finally (return (coerce gradients 'vector))))
            
            (t (error "Unsupported parameter type: ~A" param))))))

(defun revise (f revs ϑ)
  "Apply function F iteratively REVS times, starting with initial value ϑ.

F should be a function that takes a parameter vector and returns an updated 
parameter vector. This function is commonly used for iterative optimization 
algorithms like gradient descent, where F represents a single parameter 
update step.

Arguments:
  F    - A function that takes a parameter vector and returns an updated one
  REVS - Number of iterations to perform (non-negative integer)
  ϑ    - Initial parameter vector

Returns the final parameter vector after REVS iterations.

Example:
  ;; Apply gradient descent updates 1000 times
  (revise update-function 1000 '(0.0 0.0))"
  (cond
    ((zerop revs) ϑ)
    (t (revise f (1- revs) (funcall f ϑ)))))

;;(defun gradient-descent (obj-fun ϑ &key (α 0.01) (revs 1000))
(defun gradient-descent (obj-fun ϑ)
  "Minimize OBJ-FUN using gradient descent optimization starting from initial parameters ϑ.

Gradient descent is an iterative optimization algorithm that finds local minima
by repeatedly moving in the direction of steepest descent (negative gradient).
At each iteration, parameters are updated according to the rule:
  ϑ_new = ϑ_old - α * ∇f(ϑ_old)

Arguments:
  OBJ-FUN - Objective function to minimize (should return a scalar)
  ϑ       - Initial parameter vector (list of numbers)

Keyword Arguments:
  α    - Learning rate/step size (default: 0.01). Controls how large steps
         to take in the direction of the negative gradient. Too large values
         may cause overshooting; too small values slow convergence.
  REVS - Number of iterations to perform (default: 1000)

Returns the optimized parameter vector after REVS iterations.

The function uses numerical differentiation to approximate gradients, which
may be less accurate than analytical gradients but works for any differentiable
function.

Example:
  ;; Minimize a quadratic loss function
  (gradient-descent loss-function '(0.0 0.0) :α 0.001 :revs 5000)"
  (let ((f (lambda (θ)
             (mapcar (lambda (p g)
                       (t- p (t* *α* g)))
                     θ (compute-gradients obj-fun θ)))))
    (revise f *revs* ϑ)))

#|

Full example:

(with-hypers
         ((*revs* 100)
          (*α* 0.001))
       (gradient-descent 
        (l2-loss (coerce ina219-fnn::*training-inputs-x* 'vector)
                 (coerce ina219-fnn::*training-outputs-y* 'vector)
                 #'line)
        (list 0.0 0.0)))

|#



#|

Derivative - Rate of change
---------------------------

By increasing ϑ_0 by 0.0099 the loss was changed by -0.62.
Rate of change => -0.62 / 0.0099 => -62.63 (whole rate of change)
=> arbitrary way to find the rate of change
=> revision of ϑ_0 should not overshoot the ideal loss.

Learning rate (also known as step size, α (alpha))
---------------------------------------
Scalar multiplied with the whole rate of change:
=> α = 0.01
=> α * -62.63 = -0.6263
=> Smaller revision


Gradient
————————
=> tangient (tangente) der loss-curve.
=> slope (Gefälle/Neigung) of the tangient = gradient


|#

;; -------------------------
;; plotting
;; -------------------------

(defun seq-max (ls)
  (reduce #'max ls :initial-value 0))

(defun seq-min (ls)
  (reduce #'min ls))

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

(defun %plot-line-xs-ys--with-estimated-ϑ (xs ys &optional approx-line-xys)
  (let ((data (loop :for x :across xs
                    :for y :across ys
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
     (mgl-gnuplot:command (format nil "set xrange [~f:~f]"
                                  (- (seq-min xs) 2) (+ (seq-max xs) 2)))
     (mgl-gnuplot:command (format nil "set yrange [~f:~f]"
                                  (- (seq-min ys) 2) (+ (seq-max ys) 2)))
     (mgl-gnuplot:command "set view map"))))

(defun plot-line-xs-ys--with-estimated-ϑ ()
  "Plots the data set with an estimated line which we get ϑ from."
  (%plot-line-xs-ys--with-estimated-θ *line-xs*
                                      *line-ys*
                                      '((0.0 0.0) (4.5 4.5))))
