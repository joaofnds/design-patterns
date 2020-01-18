;;;; What is the chain of responsibility design pattern?
;;;;
;;;; - This pattern send data to an object and if that object can't use it,
;;;; it sends it to any number of other objects that may be able to use it
;;;;    - Create 4 objects that can either add, subtract, multiply, or divide
;;;;    - Send 2 numbers and a command and allow these 4 object to decide
;;;;      which can handle the requested calculation
;;;;
;;;; @see http://www.newthinktank.com/2012/10/chain-of-responsibility-design-pattern-tutorial/

(defclass numbers ()
  ((a  :reader a  :initarg :a  :initform (error "must have 'a'"))
   (b  :reader b  :initarg :b  :initform (error "must have 'b'"))
   (op :reader op :initarg :op :initform (error "must have 'op'"))))

(defclass end-of-chain () nil)

(defclass add-numbers ()
  ((next-in-chain :accessor next-in-chain :initarg :next-in-chain)))

(defclass subtract-numbers ()
  ((next-in-chain :accessor next-in-chain :initarg :next-in-chain)))

(defclass multiply-numbers ()
  ((next-in-chain :accessor next-in-chain :initarg :next-in-chain)))

(defclass divide-numbers ()
  ((next-in-chain :accessor next-in-chain :initarg :next-in-chain)))

(defgeneric calculate (operator numbers))

(defmethod calculate ((operator add-numbers) request)
  (if (eq 'add (op request))
      (with-slots (a b) request
        (format t "~a + ~a = ~a~%" a b (+ a b)))
      (calculate (next-in-chain operator) request)))

(defmethod calculate ((operator subtract-numbers) request)
  (if (eq 'subtract (op request))
      (with-slots (a b) request
        (format t "~a - ~a = ~a~%" a b (- a b)))
      (calculate (next-in-chain operator) request)))

(defmethod calculate ((operator multiply-numbers) request)
  (if (eq 'multiply (op request))
      (with-slots (a b) request
        (format t "~a * ~a = ~a~%" a b (* a b)))
      (calculate (next-in-chain operator) request)))

(defmethod calculate ((operator divide-numbers) request)
  (if (eq 'divide (op request))
      (with-slots (a b) request
        (format t "~a / ~a = ~a~%" a b (/ a b)))
      (calculate (next-in-chain operator) request)))

(defmethod calculate ((operator end-of-chain) numbers)
  (format t "reached end of chain"))

;;;; ---------------------------------------------------------------------------

(defparameter *chain-calc-1* (make-instance 'add-numbers))
(defparameter *chain-calc-2* (make-instance 'subtract-numbers))
(defparameter *chain-calc-3* (make-instance 'multiply-numbers))
(defparameter *chain-calc-4* (make-instance 'divide-numbers))
(defparameter *chain-calc-5* (make-instance 'end-of-chain))

(setf (next-in-chain *chain-calc-1*) *chain-calc-2*)
(setf (next-in-chain *chain-calc-2*) *chain-calc-3*)
(setf (next-in-chain *chain-calc-3*) *chain-calc-4*)
(setf (next-in-chain *chain-calc-4*) *chain-calc-5*)

(calculate *chain-calc-1* (make-instance 'numbers :a 1 :b 2 :op 'add))
(calculate *chain-calc-1* (make-instance 'numbers :a 1 :b 2 :op 'subtract))
(calculate *chain-calc-1* (make-instance 'numbers :a 1 :b 2 :op 'multiply))
(calculate *chain-calc-1* (make-instance 'numbers :a 1 :b 2 :op 'divide))
