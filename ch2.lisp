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
    (t (%ranked (first tensor) (1+ accu)))))

(test rank-test
  (is (= 0 (rank 1)))
  (is (= 1 (rank '(1))))
  (is (= 2 (rank '((1))))))

(defun shape (tensor)
  (cond
    ((numberp tensor) nil)
    (t (cons (length tensor) (shape (first tensor))))))

(test shape-test
  (is (equalp nil (shape 0)))
  (is (equalp '(1) (shape '(1))))
  (is (equalp '(4) (shape '(3 2 6 7))))
  (is (equalp '(2 1) (shape '((1) (2)))))
  (is (equalp '(2 3 1) (shape '(((5) (6) (8)) ((7) (9) (5)))))))

;; length of shape of tensor is equal to the rank of the tensor.

