(defpackage :tll.tens
  (:use :cl :fiveam)
  (:nicknames :tens)
  (:export #:rank
           #:shape
           #:t+
           #:t*
           #:t-
           #:tsquare
           #:tsqrt
           #:tdot
           #:tsum))

(in-package :tll.tens)

;;
;; tensor^0 is a scalar
;; tensor^1 is i.e.: (1 2)
;; tensor^2 is i.e. ((1) (2))
;; tensor^x where x is called the rank, how deep the elements are nested.
;; => tensor rank is the number of left square brackets before the left most scalar

(defun rank (tensor)
  "Return the rank (nesting depth) of TENSOR.

A scalar has rank 0; a one-dimensional vector rank 1; a matrix (vector of
vectors) rank 2; and so on."
  (%ranked tensor 0))

(defun %ranked (tensor accu)
  (cond
    ((numberp tensor) accu)
    (t (%ranked (aref tensor 0) (1+ accu)))))

(defun shape (tensor)
  "Return a list with the length of TENSOR along every dimension.

A scalar returns NIL; a 2x3 matrix returns '(2 3), etc."
  (cond
    ((numberp tensor) nil)
    (t (cons (length tensor) (shape (aref tensor 0))))))

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
  "Element-wise addition of TENSORS.

Each argument may be a scalar or a tensor of any rank.  Scalars are broadcast
over array arguments.  Signals a SIMPLE-ERROR when called with no arguments
or with incompatible shapes."
  (apply #'%t-op #'+ "addition" tensors))

(defun t- (&rest tensors)
  "Element-wise subtraction of TENSORS, performed left-to-right.

Broadcasting and error behaviour are identical to T+."
  (apply #'%t-op #'- "subtraction" tensors))

(defun t* (&rest tensors)
  "Element-wise multiplication of TENSORS.

Behaves like T+ with respect to broadcasting and error signalling."
  (apply #'%t-op #'* "multiplication" tensors))

(defun tsquare (tensor)
  "Return the element-wise square of TENSOR.

TENSOR may be a scalar or any-rank tensor. Signals a SIMPLE-ERROR on 
wrong argument count or shape mismatch."
  (%t-op #'* "square" tensor tensor))

(defun tsqrt (&rest tensors)
  "Return the element-wise square root of TENSOR.

Exactly one argument must be supplied; it may be a scalar or any-rank tensor.
Signals a SIMPLE-ERROR on wrong argument count or shape mismatch."
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

(defun tdot (we te)
  ;;(assert (equalp (shape we) (shape te)))
  (tsum
   (t* we te)))

(defun %sum-1 (tensor)
  (%summed tensor (1- (length tensor)) 0.0))

(defun %summed (tensor i accu)
  (cond
    ((zerop i) (+ (aref tensor 0) accu))
    (t (%summed tensor (1- i) (+ (aref tensor i) accu)))))

(defun tsum (&rest tensors)
  "Sum across the first dimension (axis 0) of TENSOR, reducing its rank by one.

Exactly one argument must be supplied and it must be an array of rank ≥ 1.
Signals a SIMPLE-ERROR when called with the wrong argument count or with a
scalar."
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


;;; --------------------------------
;;; tests
;;; --------------------------------

(test rank-test
  (is (= 0 (rank 1)))
  (is (= 1 (rank #(1))))
  (is (= 2 (rank #(#(1))))))

(test shape-test
  (is (equalp nil (shape 0)))
  (is (equalp '(1) (shape #(1))))
  (is (equalp '(4) (shape #(3 2 6 7))))
  (is (equalp '(2 1) (shape #(#(1) #(2)))))
  (is (equalp '(2 3 1) (shape #(#(#(5) #(6) #(8)) #((7) #(9) #(5)))))))

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

(test square-test
  ;; scalar tests
  (is (= 4 (tsquare 2)))
  (is (= 9 (tsquare 3)))
  (is (= 0 (tsquare 0)))
  (is (= 1 (tsquare -1)))
  (is (= 4 (tsquare -2)))
  
  ;; rank-1 tensor tests
  (is (equalp #(1 4 9) (tsquare #(1 2 3))))
  (is (equalp #(0 1 4) (tsquare #(0 1 2))))
  (is (equalp #(1 4 9 16) (tsquare #(-1 -2 3 4))))
  
  ;; rank-2 tensor tests
  (is (equalp #(#(1 4) #(9 16))
              (tsquare #(#(1 2) #(3 4)))))
  (is (equalp #(#(0 1) #(4 9))
              (tsquare #(#(0 1) #(2 3)))))
  (is (equalp #(#(1 4) #(9 16))
              (tsquare #(#(-1 -2) #(3 4)))))
  
  ;; rank-3 tensor tests
  (is (equalp #(#(#(1 4) #(9 16)) #(#(25 36) #(49 64)))
              (tsquare #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))))))
  
  ;; edge cases
  (is (equalp #() (tsquare #()))))  ; empty vector

(test sqrt-test
  (is (= 2 (tsqrt 4)))
  (is (equalp #(2 3) (tsqrt #(4 9))))
  (is (equalp #(#(2) #(3))
              (tsqrt #(#(4) #(9)))))
  (signals simple-error (tsqrt))
  (signals simple-error (tsqrt 1 2)))

(test dot-test
  (is (= 41.0 (tdot #(2.0 1.0 7.0) #(8.0 4.0 3.0))))
  (is (equalp #(41.0) (tdot #(#(2.0 1.0 7.0)) #(#(8.0 4.0 3.0)))))
  )

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
(run! 'dot-test)
(run! 'sum-1-test)
(run! 'sum-test)

