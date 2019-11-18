;;;; What is the Decorator Pattern?
;;;;
;;;; - The Decorator allows you to modify an object dynamically
;;;; - You would use it when you want the capabilities of inheritance with
;;;;   subclasses, but you need to add functionality at run time
;;;; - It is more flexible than inheritance
;;;; - Simplifies code because you add functionality using many simple classes
;;;; - Rather then rewrite old code, you can extend with new code
;;;;
;;;; @see http://www.newthinktank.com/2012/09/decorator-design-pattern-tutorial/

(defgeneric description (pizza))
(defgeneric cost (pizza))

(defclass plain-pizza () ())

(defmethod description ((pizza plain-pizza))
  "Thin Dough")
(defmethod cost ((pizza plain-pizza))
  400)

(defclass topping-decorator ()
  ((temp-pizza
    :initarg :pizza
    :initform (error "must have a pizza")
    :reader temp-pizza)))

(defmethod description ((pizza topping-decorator))
  (description (temp-pizza pizza)))

(defmethod cost ((pizza topping-decorator))
  (cost (temp-pizza pizza)))


(defclass mozzarella (topping-decorator) ())

(defmethod description ((pizza mozzarella))
  (concatenate 'string (call-next-method) ", Mozzarella"))

(defmethod cost ((pizza mozzarella))
  (+ (call-next-method) 50))


(defclass tomato-sauce (topping-decorator) ())

(defmethod description ((pizza tomato-sauce))
  (concatenate 'string (call-next-method) ", Tomato Sauce"))

(defmethod cost ((pizza tomato-sauce))
  (+ (call-next-method) 35))

;;------------------------------------------------------------------------------

(let ((basic-pizza (make-instance 'tomato-sauce :pizza (make-instance 'mozzarella :pizza (make-instance 'plain-pizza)))))
  (format t "ingredients: ~a~%" (description basic-pizza))
  (format t "price: ~a~%" (cost basic-pizza)))
