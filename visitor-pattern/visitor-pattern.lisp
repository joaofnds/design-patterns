;;;; What is the visitor design pattern?
;;;;
;;;; - Allows you to add methods to classes of different types without much
;;;;   altering to those classes
;;;; - You can make completely different methods depending on the class used
;;;; - Allows you to define external classes that extend other classes without
;;;;   majorly editing them
;;;;
;;;; @see http://www.newthinktank.com/2012/11/visitor-design-pattern-tutorial/

(defclass item ()
  ((price
    :initarg :price
    :initform (error "must have price")
    :accessor price)))

(defclass liquor (item) nil)
(defclass tobacco (item) nil)
(defclass necessity (item) nil)

(defclass tax-visitor nil nil)
(defclass tax-holiday-visitor nil nil)

(defgeneric accept (item visitor))
(defmethod accept ((item item) visitor)
  (visit visitor item))

(defgeneric visit (visitor item))
(defmethod visit ((visitor tax-visitor) (item liquor))
  (with-accessors ((price price)) item
    (incf price (* price 0.18))))

(defmethod visit ((visitor tax-holiday-visitor) (item liquor))
  (with-accessors ((price price)) item
    (incf price (* price 0.36))))

(defmethod visit ((visitor tax-visitor) (item tobacco))
  (with-accessors ((price price)) item
    (incf price (* price 0.21))))

(defmethod visit ((visitor tax-holiday-visitor) (item tobacco))
  (with-accessors ((price price)) item
    (incf price (* price 0.42))))

(defmethod visit ((visitor tax-visitor) (item necessity))
  (price item))

(defmethod visit ((visitor tax-holiday-visitor) (item necessity))
  (price item))

;;;; ---------------------------------------------------------------------------

(defparameter *liquor* (make-instance 'liquor :price 10))
(defparameter *tobacco* (make-instance 'tobacco :price 10))
(defparameter *necessity* (make-instance 'necessity :price 10))

(defparameter *tax-visitor* (make-instance 'tax-visitor))
(format t "[after tax] liquor: ~a~%" (accept *liquor* *tax-visitor*))
(format t "[after tax] tobacco: ~a~%" (accept *tobacco* *tax-visitor*))
(format t "[after tax] necessity: ~a~%" (accept *necessity* *tax-visitor*))

(setf (price *liquor*) 10
      (price *tobacco*) 10
      (price *necessity*) 10)
(defparameter *tax-holiday-visitor* (make-instance 'tax-holiday-visitor))
(format t "[after holiday tax] liquor: ~a~%" (accept *liquor* *tax-holiday-visitor*))
(format t "[after holiday tax] tobacco: ~a~%" (accept *tobacco* *tax-holiday-visitor*))
(format t "[after holiday tax] necessity: ~a~%" (accept *necessity* *tax-holiday-visitor*))
