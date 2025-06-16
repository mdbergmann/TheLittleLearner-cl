(defpackage :ina219-fnn
  (:use :cl :mgl)
  ;;(:export #:)
  )

(in-package :ina219-fnn)

(defparameter *training-inputs-x*
  '(7.8 10.1 10.6 11.0 11.1 11.6 12.3 12.6 13.1))
(defparameter *training-outputs-y*
  '(30.0 61.0 67.0 73.5 74.0 81.0 89.0 94.0 100.0))

(defparameter *validation-inputs-x*
  '(7.8 9.7 10.1 10.4 10.9 11.5 12.1 12.5 13.1))
(defparameter *validation-outputs-y*
  '(30.0 54.3 59.6 63.6 70.2 78.2 86.2 91.6 100.0))

(defclass ina219-fnn (fnn) ())

(defvar *fnn* nil)
(defvar *training-data* nil)
(defvar *validation-data* nil)
(defparameter *lowest-mse* '(99999.0 0))

(defparameter *best-model-state-path* #P"ina219-learner-best.state")

(defun make-ina219-fnn (&key (n-hiddens 1))
  (build-fnn (:class 'ina219-fnn)
    (input (->input :size 1))

    (hidden-activation (->activation input :size n-hiddens))
    ;;(hidden (->sigmoid hidden-activation))
    (hidden (->relu hidden-activation)) ;; best choice for this problem
    ;;(hidden (->tanh hidden-activation))
    
    (weights (->weight :dimensions (list n-hiddens 1)))
    (output (->v*m hidden weights))
    
    (target (->input :size 1))
    (loss (->loss (->squared-difference output target)
                  :name 'squared-error))
    ))

(defmethod set-input (instances (fnn ina219-fnn))
  (let ((input-nodes (nodes (find-clump 'input fnn))))
    (fill! 0 input-nodes)
    (loop :for i :upfrom 0
          :for instance :in instances
          :do
             (cond
               ((listp instance)
                (let ((target-nodes (nodes (find-clump 'target fnn))))
                  (destructuring-bind (x y) instance
                    (setf (mref input-nodes i 0)
                          (coerce x 'double-float))
                    (setf (mref target-nodes i 0)
                          (coerce y 'double-float)))))
               (t
                (setf (mref input-nodes i 0)
                      (coerce instance 'double-float))
                )))))

(defmethod learning-rate ((optimizer sgd-optimizer))
  (call-next-method))

(defun train-ina219-fnn (&key epochs)
  (let* ((batch-size (length *training-data*))
         (optimizer
           (make-instance 'segmented-gd-optimizer
                          :segmenter
                          (constantly
                           (make-instance 'sgd-optimizer
                                          :learning-rate 0.001
                                          :Momentum 0.9
                                          :batch-size batch-size)))))

    ;; Initialize weights with small random values
    (map-segments (lambda (weights)
                    (gaussian-random! (nodes weights) :stddev .01))
                  *fnn*)

    (setf (max-n-stripes *fnn*) batch-size)

    (monitor-optimization-periodically
     optimizer `((:fn capture-training-y :period 500)
                 (:fn log-test-error :period ,(* 10 batch-size))
                 (:fn reset-optimization-monitors :period 1000)))
    (minimize optimizer
              (make-instance 'bp-learner
                             :bpn *fnn*
                             :monitors (make-cost-monitors
                                        *fnn* :attributes `(:event "train")))
              :dataset (make-sampler *training-data*
                                     (* batch-size epochs)
                                     batch-size))
    ))

(defun make-sampler (instances max-n-samples batch-size)
  (make-instance 'function-sampler
                 :max-n-samples max-n-samples
                 :generator (lambda ()
                              (nth (random batch-size) instances))))

(defparameter *captured-ys* nil)

(defun capture-training-y (optimizer learner)
  (declare (ignore optimizer learner))
  (push
   (mapcar #'predict-with-fnn
           (mapcar #'normalize-x *training-inputs-x*))
   *captured-ys*))

(defun log-test-error (optimizer learner)
  (when (zerop (n-instances optimizer))
    (describe optimizer)
    (describe (bpn learner)))
  (let* ((test-samples 100)
         (test-batch-size (length *validation-data*))
         (monitors
           (monitor-bpn-results (make-sampler *validation-data* test-samples test-batch-size)
                                (bpn learner)
                                (make-cost-monitors
                                 (bpn learner) :attributes `(:event "pred."))))
         (mse (/ (cost (bpn learner)) test-samples)))
    (when (< mse (car *lowest-mse*))
      (setf *lowest-mse* (list mse (n-instances optimizer)))
      (save-state *best-model-state-path* (bpn learner) :if-exists :overwrite)
      )
    (log-padded monitors)
    (log-msg "Mean Squared Error: ~a~%" mse)))

(defun predict-with-fnn (single-input-datum)
  (let ((fnn *fnn*))
    (setf (n-stripes fnn) 1)
    (set-input (list single-input-datum) fnn)
    (forward fnn)
    (let ((output-lump (find-clump 'output fnn)))
      (row-major-mref (nodes output-lump) 0))))

(defun evaluate-model (test-data)
  "Evaluate model on test data"
  (let ((total-loss 0.0)
        (n-samples (length test-data))
        (avg-error 0.0))
    (dolist (sample test-data)
      (destructuring-bind (feature target) sample
        (let* ((prediction (predict-with-fnn feature))
               (sample-loss (num-utils:square (abs (- prediction target)))))
          (log-msg "pred.:~a, target:~a, loss:~a~%" prediction target sample-loss)
          (incf total-loss sample-loss))))
    (setf avg-error (/ total-loss n-samples))
    (log-msg "avg-squared-error (~a samples): ~a~%" n-samples avg-error)
    avg-error))

(defun normalize-x (x)
  (/ (- x 7.0) 6.1))  ; 6.1 = 13.1 - 7.0
;;(float (/ x 10.0)))

(defun make-train-data ()
  (let* ((input-x *training-inputs-x*)
         (target-y *training-outputs-y*))
    (loop :for x :in input-x
          :for y :in target-y
          :collect (list (normalize-x x) y))))

(defun make-validation-data ()
  (let* ((input-x *validation-inputs-x*)
         (target-y *validation-outputs-y*))
    (loop :for x :in input-x
          :for y :in target-y
          :collect (list (normalize-x x) y))))

(defun generate-training-data (x-min x-max step &key
                                                  (normalize t)
                                                  (slope 13.33)
                                                  (intercept -75.0))
  "Generate training data for y = slope * x + intercept"
  (flet ((truncate-to-1-digit (x)
           (/ (truncate (* x 10)) 10.0)))
    (loop :for x := x-min :then (truncate-to-1-digit (+ x step))
          :for y := (truncate-to-1-digit (+ (* slope x) intercept))
          :until (> x x-max)
          :collect (list (if normalize (normalize-x x)
                             x)
                         y))))

(defun train (&key (epochs 500) (n-hiddens 1))
  (let ((*log-time* nil))
    (setf *fnn* (make-ina219-fnn :n-hiddens n-hiddens))
    (setf *captured-ys* nil)
    (let ((*lowest-mse* '(9999.0 0))
          (*training-data* (make-train-data)) ;; (generate-training-data 7.8 13.1 0.1))
          (*validation-data* (make-validation-data)))
      (train-ina219-fnn :epochs epochs)
      (evaluate-model *validation-data*)
      (format t "Lowest mse: ~a~%" *lowest-mse*)
      (load-state *best-model-state-path* *fnn*)
      (evaluate-model *validation-data*)
      )
    *fnn*
    ))
