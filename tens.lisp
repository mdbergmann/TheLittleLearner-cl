(defpackage :tll.tens
  (:use :cl :fiveam)
  (:nicknames :tens)
  (:export #:rank
           #:shape
           #:t+
           #:t*
           #:t-
           #:tsqrt
           #:tsum))

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

(defun %t-op (op error-topic &rest tensors)
  (assert (> (length tensors) 0))
  (labels ((tt (a b)
             (cond
               ((numberp a)
                (if (numberp b)
                    (funcall op a b)
                    (map 'vector (lambda (x) (tt a x)) b)))
               ((numberp b)
                (map 'vector (lambda (x) (tt x b)) a))
               ((and (arrayp a) (arrayp b))
                (map 'vector #'tt a b))
               (t (error "Incompatible tensor shapes for ~A." error-topic)))))
    (reduce #'tt tensors)))


(defun t+ (&rest tensors)
  (apply #'%t-op #'+ "addition" tensors))

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

(defun t- (&rest tensors)
  (apply #'%t-op #'- "subtraction" tensors))

(test minus-test
  (is (= -1 (t- 1 2)))
  (is (equalp #(-2 -2) (t- #(1 2) #(3 4))))

  ;; scalar vs. vector
  (is (equalp #(0 -1 -2) (t- 1 #(1 2 3))))
  (is (equalp #(0 1 2) (t- #(1 2 3) 1)))

  ;; rank-2 tests
  (is (equalp #(#(0 0) #(0 0))
              (t- #(#(1 2) #(3 4))
                  #(#(1 2) #(3 4)))))
  (is (equalp #(#(-5 -3 -1) #(1 3 5))
              (t- #(#(1 2 3) #(4 5 6))
                  #(#(6 5 4) #(3 2 1)))))

  ;; rank-3 test
  (is (equalp
        #(#(#(0 0) #(0 0))
          #(#(0 0) #(0 0)))
        (t-
          #(#(#(1 1) #(2 2))  #(#(3 3) #(4 4)))
          #(#(#(1 1) #(2 2))  #(#(3 3) #(4 4))))))

  (signals simple-error (t-)))

(defun t* (&rest tensors)
  (apply #'%t-op #'* "multiplication" tensors))

(test multiply-test
  (is (= 2 (t* 1 2)))
  (is (equalp #(3 8) (t* #(1 2) #(3 4))))
  (is (equalp #(#(1) #(4))
              (t* #(#(1) #(2)) #(#(1) #(2)))))
  (is (equalp #(#(5 12)
                #(21 32))
              (t* #(#(1 2)
                    #(3 4))
                  #(#(5 6)
                    #(7 8)))))
  (is (equalp #(12 24 20) (t* 4 #(3 6 5))))
  (signals simple-error (t*)))

(defun tsqrt (&rest tensors)
  (assert (= (length tensors) 1))
  (labels ((ts (a)
             (cond
               ((numberp a)
                (sqrt a))
               ((arrayp a)
                (map 'vector #'ts a))
               (t
                (error "Incompatible tensor shapes for ~A." "square root")))))
    (ts (first tensors))))

(test sqrt-test
  (is (= 2 (tsqrt 4)))
  (is (equalp #(2 3) (tsqrt #(4 9))))
  (is (equalp #(#(2) #(3))
              (tsqrt #(#(4) #(9)))))
  (signals simple-error (tsqrt))
  (signals simple-error (tsqrt 1 2)))

(defun %sum-1 (tensor)
  (%summed tensor (1- (length tensor)) 0.0))

(defun %summed (tensor i accu)
  (cond
    ((zerop i) (+ (aref tensor 0) accu))
    (t (%summed tensor (1- i) (+ (aref tensor i) accu)))))

(defun tsum (&rest tensors)
  "Sum across the first dimension, reducing rank by 1."
  (assert (= (length tensors) 1))
  (labels ((ts (a)
             (cond
               ((arrayp a)
                (if (= (rank a) 1)
                    (%sum-1 a)
                    (map 'vector #'ts a)))
               (t
                (error "Incompatible tensor shapes for ~A." "summation")))))
    (ts (first tensors))))

(test sum-test
  (is (= 6.0 (tsum #(1.0 2.0 3.0))))
  (is (equalp #(3.0 7.0) (tsum #(#(1.0 2.0) #(3.0 4.0)))))
  (is (equalp #(#(3.0) #(7.0))
              (tsum #(#(#(1.0 2.0)) #(#(3.0 4.0))))))
  (signals simple-error (tsum))
  (signals simple-error (tsum 1 2))
  (signals simple-error (tsum 1)))

(test sum-1-test
  (is (= 36.0 (%sum-1 #(10.0 12.0 14.0)))))


;;; ----- running tests --------

(run! 'rank-test)
(run! 'shape-test)
(run! 'plus-test)
(run! 'minus-test)
(run! 'multiply-test)
(run! 'sqrt-test)
(run! 'sum-1-test)
(run! 'sum-test)

