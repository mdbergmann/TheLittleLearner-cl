(defpackage :tll.ch2
  (:use :cl :fiveam)
  (:nicknames :ch2)
  ;;(:export #:)
  )

(in-package :tll.ch2)

;; length of shape of tensor is equal to the rank of the tensor.

;;; ------------------------
;;; Interlude I -- +
;;; ------------------------

;; summed tensor is rank-1 of the original tensor

(defun sum-1 (tensor)
  (%summed tensor (1- (length tensor)) 0.0))

(defun %summed (tensor i accu)
  (cond
    ((zerop i) (+ (aref tensor 0) accu))
    (t (%summed tensor (1- i) (+ (aref tensor i) accu)))))

(test sum-1-test
  (is (= 36.0 (sum-1 #(10.0 12.0 14.0)))))


(run! 'sum-1-test)
