(defpackage :ina219-fnn
  (:use :cl :mgl)
  ;;(:export #:)
  )

(in-package :ina219-fnn)

(defparameter *training-inputs-x*
  '(7.8 10.1 10.6 11.0 11.1 11.6 12.3 12.6 13.1))
(defparameter *training-outputs-y*
  '(30.0 61.0 67.0 73.5 74.0 81.0 89.0 94.0 100.0))

(defclass ina219-fnn (fnn) ())

(defvar *fnn* nil)
(defvar *training-data* nil)
(defvar *test-data* nil)
(defparameter *lowest-avg-loss* '(99999.0 0))

(defun make-ina219-fnn (&key (n-hiddens 1))
  (build-fnn (:class 'ina219-fnn)
    (input (->input :size 1))

    (hidden-activation (->activation input :size n-hiddens))
    ;;(hidden (->sigmoid hidden-activation))
    (hidden (->relu hidden-activation)) ;; best choice for this problem
    ;;(hidden (->tanh hidden-activation))
    
    (weights (->weight :dimensions (list n-hiddens 1))) ;; :size (* n-hiddens 1)))
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
                    (gaussian-random! (nodes weights) :stddev 0.01))
                  *fnn*)

    (setf (max-n-stripes *fnn*) batch-size)

    (monitor-optimization-periodically
     optimizer `((:fn log-test-error :period ,(* 10 batch-size))
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

(defun log-test-error (optimizer learner)
  (when (zerop (n-instances optimizer))
    (describe optimizer)
    (describe (bpn learner)))
  (let ((avg-error (evaluate-model *test-data*)))
    (when (< avg-error (car *lowest-avg-loss*))
      (setf *lowest-avg-loss* (list avg-error (n-instances optimizer))))
    (log-padded
     (monitor-bpn-results (make-sampler *test-data* 100 (length *test-data*))
                          (bpn learner)
                          (make-cost-monitors
                           (bpn learner) :attributes `(:event "pred."))))))

(defun predict-with-fnn (input-data)
  (let ((fnn *fnn*))
    (setf (n-stripes fnn) (length input-data))
    (set-input input-data fnn)
    (forward fnn)
    (let ((output-lump (find-clump 'output fnn)))
      (nodes output-lump))))

(defun evaluate-model (test-data)
  "Evaluate model on test data"
  (let ((total-loss 0.0)
        (n-samples (length test-data))
        (avg-error 0.0))
    (dolist (sample test-data)
      (destructuring-bind (feature target) sample
        (let* ((prediction (row-major-mref
                            (predict-with-fnn (list feature))
                            0))
               (sample-loss (num-utils:square (abs (- prediction target)))))
          (log-msg "pred.:~a, target:~a, loss:~a~%" prediction target sample-loss)
          (incf total-loss sample-loss))))
    (setf avg-error (/ total-loss n-samples))
    (log-msg "avg-squared-error (~a samples): ~a~%" n-samples avg-error)
    avg-error))

(defun normalize-x (x) ;;x)
  (float (/ x 10.0)))

(defun make-test-data ()
  (let* ((input-x *training-inputs-x*)
         (target-y *training-outputs-y*))
    (loop :for x :in input-x
          :for y :in target-y
          :collect (list (normalize-x x) y))))

(defun generate-training-data (x-min x-max step &key
                                                  (slope 13.33)
                                                  (intercept -75.0))
  "Generate training data for y = slope * x + intercept"
  (flet ((truncate-to-1-digit (x)
           (/ (truncate (* x 10)) 10.0)))
    (loop :for x := x-min :then (truncate-to-1-digit (+ x step))
          :for y := (truncate-to-1-digit (+ (* slope x) intercept))
          :until (> x x-max)
          :collect (list (normalize-x x) y))))

(defun train (&key (epochs 500) (n-hiddens 1))
  (let ((*log-time* nil))
    (setf *fnn* (make-ina219-fnn :n-hiddens n-hiddens))
    (let ((*lowest-avg-loss* '(9999.0 0))
          (*training-data* (generate-training-data 7.8 13.1 0.1))
          (*test-data* (make-test-data)))
      (train-ina219-fnn :epochs epochs)
      (evaluate-model *test-data*)
      (format t "Lowest avg loss: ~a~%" *lowest-avg-loss*))
    *fnn*
    ))
