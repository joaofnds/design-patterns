;;;; What is the interpreter design pattern?
;;;;
;;;; - It is used to convert one representation of data into another
;;;; - The Context contains the information that will be interpreted
;;;; - The Expression is an abstract class that defines all the methods
;;;;   needed to perform the different conversions
;;;; - The Terminal or Concrete Expressions provide specific conversions on
;;;;   different types of data
;;;;
;;;; @see http://www.newthinktank.com/2012/10/interpreter-design-pattern-tutorial/

(ql:quickload "cl-ppcre")

(defun last-char (str)
  (char str (- (length str) 1)))

(defun enforce-plural (str)
  (if (char= #\s (last-char str))
      str
      (format nil "~as" str)))

(defclass conversion ()
  ((from-unit :accessor from-unit)
   (to-unit   :accessor to-unit)
   (amount    :accessor amount)))

(defmethod initialize-instance :after ((class conversion) &key question)
  (let ((words (cl-ppcre:split #\Space question)))
    (setf (amount class) (parse-integer (elt words 0)))
    (setf (from-unit class) (enforce-plural (elt words 1)))
    (setf (to-unit class) (enforce-plural (elt words 3)))))

(defmethod compute-response ((converter conversion))
  (with-slots (amount from-unit to-unit) converter
    (let* ((expression (make-expression from-unit))
           (result (apply (intern (string-upcase to-unit)) (list expression amount))))
      (format nil "~a ~a equals ~a ~a" amount from-unit result to-unit))))

(defgeneric bits (unit amount))
(defgeneric nibbles (unit amount))
(defgeneric bytes (unit amount))

(defclass _bit () ())
(defmethod bits ((unit _bit) amount)
  amount)
(defmethod nibbles ((unit _bit) amount)
  (/ amount 4))
(defmethod bytes ((unit _bit) amount)
  (/ amount 8))

(defclass nibble () nil)
(defmethod bits ((unit nibble) amount)
  (* amount 4))
(defmethod nibbles ((unit nibble) amount)
  amount)
(defmethod bytes ((unit nibble) amount)
  (/ amount 2))

(defclass _byte () nil)
(defmethod bits ((unit _byte) amount)
  (* amount 8))
(defmethod nibbles ((unit _byte) amount)
  (* amount 2))
(defmethod bytes ((unit _byte) amount)
  amount)

(defmethod make-expression (unit)
  (cond
    ((string= unit "bits") (make-instance '_bit))
    ((string= unit "nibbles") (make-instance 'nibble))
    ((string= unit "bytes") (make-instance '_byte))))

;;;; ---------------------------------------------------------------------------

(defparameter *conversion* (make-instance 'conversion :question "32 bits to bytes"))
(print (compute-response *conversion*))
