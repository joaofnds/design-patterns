;;;; What is the Template Method Design Pattern?
;;;;
;;;; - Used to create a group of subclasses that have to execute
;;;;   a similar group of method
;;;; - You create an abstract class that contains a method called
;;;;   the Template Method
;;;; - The Template Method contains a series of method calls that
;;;;   every subclass object will call
;;;; - The subclass object can override some of the method calls
;;;;
;;;; @see http://www.newthinktank.com/2012/10/template-method-design-pattern-tutorial/

(defclass hoagie () ())

(defclass italian-hoagie (hoagie)
  ((meat :initform '("salami" "pepperoni" "capicola ham") :reader meat)
   (cheese :initform '("provolone") :reader cheese)
   (veggies :initform '("lettuce" "tomatoes" "onions" "weet peppers") :reader veggies)
   (condiments :initform '("oil vinegar") :reader condiments)))

(defclass veggie-hoagie (hoagie)
  ((veggies :initform '("lettuce" "tomatoes" "onions" "sweet peppers") :reader veggies)
   (condiments :initform '("oil" "vinegar") :reader condiments)))

(defgeneric add-meat (hoagie))
(defmethod add-meat ((hoagie italian-hoagie))
  (format t "adding meat: ~{~a~^, ~}~%" (meat hoagie)))

(defgeneric add-cheese (hoagie))
(defmethod add-cheese ((hoagie italian-hoagie))
  (format t "adding cheese: ~{~a~^, ~}~%" (cheese hoagie)))

(defgeneric add-vegetables (hoagie))
(defmethod add-vegetables ((hoagie italian-hoagie))
  (format t "adding vegetables: ~{~a~^, ~}~%" (veggies hoagie)))
(defmethod add-vegetables ((hoagie veggie-hoagie))
  (format t "adding vegetables: ~{~a~^, ~}~%" (veggies hoagie)))

(defgeneric add-condiments (hoagie))
(defmethod add-condiments ((hoagie italian-hoagie))
  (format t "adding condiments: ~{~a~^, ~}~%" (condiments hoagie)))
(defmethod add-condiments ((hoagie veggie-hoagie))
  (format t "adding condiments: ~{~a~^, ~}~%" (condiments hoagie)))

(defgeneric make-sandwich (hoagie))
(defmethod make-sandwich ((hoagie hoagie))
  (cut-bun hoagie)
  (when (customer-wants-meat hoagie) (add-meat hoagie))
  (when (customer-wants-cheese hoagie) (add-cheese hoagie))
  (when (customer-wants-vegetables hoagie) (add-vegetables hoagie))
  (when (customer-wants-condiments hoagie) (add-condiments hoagie))
  (wrap-the-hoagie hoagie))

(defgeneric cut-bun (hoagie))
(defmethod cut-bun ((hoagie hoagie))
  (format t "the hoagie is cut~%"))

(defgeneric wrap-the-hoagie (hoagie))
(defmethod wrap-the-hoagie ((hoagie hoagie))
  (format t "wrap the hoagie~%"))

(defgeneric customer-wants-meat (hoagie))
(defmethod customer-wants-meat ((hoagie hoagie)) t)
(defmethod customer-wants-meat ((hoagie veggie-hoagie)) nil)

(defgeneric customer-wants-cheese (hoagie))
(defmethod customer-wants-cheese ((hoagie hoagie)) t)
(defmethod customer-wants-cheese ((hoagie veggie-hoagie)) nil)

(defgeneric customer-wants-vegetables (hoagie))
(defmethod customer-wants-vegetables ((hoagie hoagie)) t)

(defgeneric customer-wants-condiments (hoagie))
(defmethod customer-wants-condiments ((hoagie hoagie)) t)


;;;; ---------------------------------------------------------------------------

(format t "##########################~%")
(format t "##### Italian Hoagie #####~%")
(format t "##########################~%")
(let ((italian-hoagie (make-instance 'italian-hoagie)))
  (make-sandwich italian-hoagie))

(format t "~%")

(format t "#########################~%")
(format t "##### Veggie Hoagie #####~%")
(format t "#########################~%")
(let ((veggie-hoagie (make-instance 'veggie-hoagie)))
  (make-sandwich veggie-hoagie))
