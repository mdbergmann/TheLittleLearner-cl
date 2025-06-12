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

(defun make-ina219-fnn (&key (n-inputs 1) (n-hiddens 1) (n-outputs 1))
  (build-fnn (:class 'ina219-fnn)
    (input (->input :size n-inputs))

    (weights (->weight :size (* n-hiddens n-inputs)))
    (output (->v*m input weights))
    
    (target (->input :size n-outputs))
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
                (setf (mref input-nodes i 0) (coerce instance 'double-float))
                )))))

(defun train-ina219-fnn (fnn training-data &key (epochs 50))
  (let* ((batch-size (length training-data))
         (optimizer
           (make-instance 'segmented-gd-optimizer
                          :segmenter
                          (constantly
                           (make-instance 'sgd-optimizer
                                          :learning-rate 0.01
                                          :Momentum 0.9
                                          :batch-size batch-size)))))
    (setf (max-n-stripes fnn) batch-size)
    (monitor-optimization-periodically
     optimizer '((:fn reset-optimization-monitors :period 1000)))
    (minimize optimizer
              (make-instance 'bp-learner
                             :bpn fnn
                             :monitors (make-cost-monitors
                                        fnn :attributes `(:event "train")))
              :dataset
              (make-instance 'function-sampler
                             :max-n-samples
                             (* (length training-data) epochs)
                             :generator
                             (lambda ()
                               (nth (random (length training-data)) 
                                    training-data))))))

(defun predict-with-fnn (input-data)
  (let ((fnn *fnn*))
    (setf (n-stripes fnn) (length input-data))
    (set-input input-data fnn)
    (forward fnn)
    (let ((output-lump (find-clump 'output fnn)))
      (nodes output-lump))))

(defun make-training-data ()
  (let* ((input-x *training-inputs-x*)
         (target-y *training-outputs-y*))
    (loop :for x :in input-x
          :for y :in target-y
          :collect (list x y))))

(defun generate-training-data (x-min x-max step &key
                                                        (slope 13.33)
                                                        (intercept -75.0))
  "Generate training data for y = slope * x + intercept"
  (flet ((truncate-to-1-digit (x)
           (/ (truncate (* x 10)) 10.0)))
    (loop :for x := x-min :then (truncate-to-1-digit (+ x step))
          :for y := (truncate-to-1-digit (+ (* slope x) intercept))
          :until (> x x-max)
          :collect (list x
                         y))))

(defun train (&key (epochs 500))
  (let ((*log-time* nil))
    (setf *fnn* (make-ina219-fnn :n-inputs 1 :n-outputs 1))
    (train-ina219-fnn *fnn* (generate-training-data 7.8 13.1 0.1)
                      :epochs epochs)
    (let (;;(hidden (nodes (find-clump 'hidden *fnn*)))
          (loss (nodes (find-clump 'squared-error *fnn*))))
      ;;(format t "Hidden: ~a~%" hidden)
      (format t "Squared error: ~a~%" loss)
      )
    *fnn*
    ))
