(defpackage :tll.tens
  (:use :cl :fiveam)
  (:nicknames :tens)
  (:export #:rank
           #:shape))

(in-package :tll.tens)

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


(defun t+ (&rest tensors)
  (assert (> (length tensors) 0))
  (labels ((add-tensors (a b)
             (cond
               ((numberp a)
                (if (numberp b)
                    (+ a b)
                    (map 'vector (lambda (x) (add-tensors a x)) b)))
               ((numberp b) (map 'vector (lambda (x) (add-tensors x b)) a))
               ((and (arrayp a) (arrayp b))
                (map 'vector #'add-tensors a b))
               (t (error "Incompatible tensor shapes for addition.")))))
    (reduce #'add-tensors tensors)))

(test plus-test
  (is (= 3 (t+ 1 2)))
  (is (equalp #(4 6) (t+ #(1 2) #(3 4))))
  (is (equalp #(#(2) #(4))
              (t+ #(#(1) #(2)) #(#(1) #(2)))))
  (is (equalp #(#(6 8)
                #(10 12))
              (t+ #(#(1 2)
                    #(3 4))
                  #(#(5 6)
                    #(7 8)))))
  (is (equalp #(7 10 9) (t+ 4 #(3 6 5))))
  (signals simple-error (t+)))

;;; ----- running tests --------

(run! 'rank-test)
(run! 'shape-test)
(run! 'plus-test)
