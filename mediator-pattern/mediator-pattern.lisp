;;;; What is the Mediator Design Pattern?
;;;;
;;;; - It is used to handle communication between related objects
;;;; - All communication is handled by the Mediator and the Colleagues
;;;;   don't need to know anything about each other
;;;; - GOF: Allows loose coupling by encapsulating the way disparate
;;;;   sets of objects interact and communicate with each other.
;;;;   Allows for the actions of each object set to vary independently
;;;;   of one another.
;;;;
;;;; @see http://www.newthinktank.com/2012/10/mediator-design-pattern-tutorial/

(defclass stock-offer ()
  ((shares
    :initarg :shares
    :initform (error "must have shares")
    :reader shares)
   (symbol
    :initarg :symbol
    :initform (error "must have symbol")
    :reader symbol)
   (colleague-code
    :initarg :colleague-code
    :initform (error "must have colleague-code")
    :reader colleague-code)))

(defclass colleague ()
  ((mediator
    :initarg :mediator
    :initform (error "must have mediator")
    :accessor mediator)
   (code :accessor code)))

(defmethod initialize-instance :after ((instance colleague) &rest initargs)
  (add-colleague (mediator instance) instance))

(defgeneric sale-offer (broker stock shares &optional code))
(defgeneric buy-offer (broker stock shares &optional code))

(defmethod sale-offer ((broker colleague) stock shares &optional code)
  (sale-offer (mediator broker) stock shares (code broker)))

(defmethod buy-offer ((broker colleague) stock shares &optional code)
  (buy-offer (mediator broker) stock shares (code broker)))

(defclass gorman-slacks (colleague) nil)
(defclass jt-poorman (colleague) nil)

(defclass stock-mediator ()
  ((colleagues  :initform nil :reader colleagues)
   (buy-offers  :initform nil :reader buy-offers)
   (sale-offers :initform nil :reader sale-offers)
   (code        :initform 0   :reader code)))

(defmethod add-colleague ((mediator stock-mediator) colleague)
  (with-slots (colleagues code) mediator
    (push colleague colleagues)
    (setf (code colleague) code)
    (incf code)))

(defmethod sale-offer ((mediator stock-mediator) stock shares &optional code)
  (let ((offer (make-instance 'stock-offer :shares shares :symbol stock :colleague-code code)))
    (with-slots (sale-offers buy-offers) mediator
      (push offer sale-offers)
      (dolist (buy-offer buy-offers)
        (when (offer-match? buy-offer offer)
          (buy mediator buy-offer sale-offer)
          (return))))))

(defmethod buy-offer ((mediator stock-mediator) stock shares &optional code)
  (let ((offer (make-instance 'stock-offer :shares shares :symbol stock :colleague-code code)))
    (with-slots (buy-offers sale-offers) mediator
      (push offer buy-offers)
      (dolist (sale-offer sale-offers)
        (when (offer-match? offer sale-offer)
          (buy mediator offer sale-offer)
          (return))))))

(defmethod offer-match? (offer1 offer2)
  (and
   (eq (symbol offer1) (symbol offer2))
   (=  (shares offer1) (shares offer2))
   (/= (colleague-code offer1) (colleague-code offer2))))

(defmethod buy ((mediator stock-mediator) buy-offer sale-offer)
  (with-slots (sale-offers buy-offers) mediator
    (remove buy-offer buy-offers)
    (remove sale-offer sale-offers)
    (format t "~a shares of ~a sold to colleague of code ~a~%"
            (shares sale-offer)
            (symbol sale-offer)
            (colleague-code buy-offer))))

;;;; ---------------------------------------------------------------------------

(defparameter *nyse* (make-instance 'stock-mediator))
(defparameter *broker1* (make-instance 'gorman-slacks :mediator *nyse*))
(defparameter *broker2* (make-instance 'jt-poorman :mediator *nyse*))

(sale-offer *broker1* 'MSFT 100)
(sale-offer *broker1* 'GOOG 50)

(sale-offer *broker2* 'NRG 10)

(buy-offer *broker1* 'NRG 10)
(buy-offer *broker2* 'MSFT 100)
