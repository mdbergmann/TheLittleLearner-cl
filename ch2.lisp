(defpackage :tll.ch2
  (:use :cl :fiveam)
  (:nicknames :ch2)
  ;;(:export #:)
  )

(in-package :tll.ch2)

;;
;; tensor^0 is a scalar
;; tensor^1 is i.e.: (1 2)
;; tensor^2 is i.e. ((1) (2))
;; tensor^x where x is called the rank, how deep the elements are nested.
;; => tensor rank is the number of left square brackets before the left most scalar

(defun rank (tensor)
  (%ranked tensor 0))

(defun %ranked (tensor accu)
  (cond
    ((numberp tensor) accu)
    (t (%ranked (aref tensor 0) (1+ accu)))))

(test rank-test
  (is (= 0 (rank 1)))
  (is (= 1 (rank #(1))))
  (is (= 2 (rank #(#(1))))))

(defun shape (tensor)
  (cond
    ((numberp tensor) nil)
    (t (cons (length tensor) (shape (aref tensor 0))))))

(test shape-test
  (is (equalp nil (shape 0)))
  (is (equalp '(1) (shape #(1))))
  (is (equalp '(4) (shape #(3 2 6 7))))
  (is (equalp '(2 1) (shape #(#(1) #(2)))))
  (is (equalp '(2 3 1) (shape #(#(#(5) #(6) #(8)) #((7) #(9) #(5)))))))

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


;;; ----- running tests --------

(run! 'rank-test)
(run! 'shape-test)
(run! 'sum-1-test)
