;; What is the strategy design pattern?
;;
;; - When you want to define a class that will have one behavior that is
;;   similar to other behaviors in a list
;; - When you need to use one of several behaviors dynamically
;; - You use this pattern if you need to dynamically change an
;;   algorithm used by an object at run time.
;; -  Allows you to eliminate code duplication
;; - Separates behavior from super and subclasses
;;
;; @see http://www.newthinktank.com/2012/08/strategy-design-pattern-tutorial/

(defclass it-flys () ())
(defclass cant-fly () ())

(defclass animal ()
  ((flying-type :accessor flying-type)))

(defclass dog (animal)
  ((flying-type :initform (make-instance 'cant-fly))))

(defclass bird (animal)
  ((flying-type :initform (make-instance 'it-flys))))

(defgeneric fly (flying-type))
(defmethod fly ((animal animal))
  (fly (flying-type animal)))
(defmethod fly ((flying-type it-flys))
  "Flying high")
(defmethod fly ((flying-type cant-fly))
  "I can't fly")

;; ------------------------------------------------------------------------------

(defparameter sparky (make-instance 'dog))
(defparameter tweety (make-instance 'bird))

(format t "sparky: ~a~%" (fly sparky))
(format t "tweety: ~a~%" (fly tweety))
(format t "setting sparky flying type...~%")
(setf (flying-type sparky) (make-instance 'it-flys))
(format t "sparky: ~a~%" (fly sparky))
