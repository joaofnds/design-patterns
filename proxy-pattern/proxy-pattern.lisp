;;;; What is the Proxy Design Pattern?
;;;;
;;;; - Provide a class which will limit access to another class
;;;; - You may do this for security reasons, because an Object is
;;;;   intensive to create, or is accessed from a remote location
;;;;
;;;; @see http://www.newthinktank.com/2012/10/proxy-design-pattern-tutorial/

;;;; code from state design pattern

;;; atm functions
(defgeneric insert-card (atm))
(defgeneric eject-card (atm))
(defgeneric insert-pin (atm pin))
(defgeneric request-cash (atm amount))

;;; atm states
(defclass has-card ()
  ((atm :initarg :atm :reader atm)))

(defclass no-card ()
  ((atm :initarg :atm :reader atm)))

(defclass has-pin ()
  ((atm :initarg :atm :reader atm)))

(defclass no-cash ()
  ((atm :initarg :atm :reader atm)))

(defclass atm ()
  ((cash            :accessor cash            :initform 2000)
   (has-correct-pin :accessor has-correct-pin :initform nil)
   (state           :accessor state)
   (has-card        :accessor has-card)
   (no-card         :accessor no-card)
   (has-pin         :accessor has-pin)
   (out-of-cash     :accessor out-of-cash)))

(defmethod initialize-instance :after ((atm atm) &key)
  (with-slots (state has-card no-card has-pin out-of-cash) atm
    (setf has-card (make-instance 'has-card :atm atm))
    (setf no-card (make-instance 'no-card :atm atm))
    (setf has-pin (make-instance 'has-pin :atm atm))
    (setf out-of-cash (make-instance 'no-cash :atm atm))
    (setf state no-card)))

;;; what the machine do with each state

;; insert card
(defmethod insert-card ((atm atm))
  (insert-card (state atm)))

(defmethod insert-card ((state has-card))
  (format t "you can't insert more than one card~%"))

(defmethod insert-card ((state no-card))
  (with-slots (atm) state
    (format t "please enter a PIN~%")
    (setf (state atm) (has-card atm))))

(defmethod insert-card ((state has-pin))
  (format t "you can't insert more than one card~%"))

(defmethod insert-card ((state no-cash))
  (format t "ATM out of cash~%"))


;; eject card
(defmethod eject-card ((atm atm))
  (eject-card (state atm)))

(defmethod eject-card ((state has-card))
  (with-slots (atm) state
    (format t "card ejected")
    (setf (state atm) (no-card atm))))

(defmethod eject-card ((state no-card))
  (format t "enter a card first~%"))

(defmethod eject-card ((state has-pin))
  (with-slots (atm) state
    (format t "card ejected~%")
    (setf (state atm) (no-card atm))))

(defmethod eject-card ((state no-cash))
  (format t "no card inserted~%"))

;; insert pin
(defmethod insert-pin ((atm atm) pin)
  (insert-pin (state atm) pin))

(defmethod insert-pin ((state has-card) pin)
  (with-slots (atm) state
    (if (= pin 1234)
        (progn
          (format t "correct PIN~%")
          (setf (has-correct-pin atm) t)
          (setf (state atm) (has-pin atm)))
        (progn
          (format t "wrong PIN")
          (setf (has-correct-pion) nil)
          (eject-card atm)))))

(defmethod insert-pin ((state no-card) pin)
  (format t "insert a card first"))

(defmethod insert-pin ((state has-pin) pin)
  (format t "already entered PIN"))

(defmethod insert-pin ((state no-cash) pin)
  (format t "ATM out of cash"))


;; request cash
(defmethod request-cash ((atm atm) amount)
  (request-cash (state atm) amount))

(defmethod request-cash ((state has-card) amount)
  (format t "enter a PIN first"))

(defmethod request-cash ((state no-card) amount)
  (format t "enter a card first"))

(defmethod request-cash ((state has-pin) amount)
  (with-slots (atm) state
    (cond
      ((not (has-correct-pin atm))
       (format t "wrong PIN")
       (eject-card atm))
      ((> amount (cash atm))
       (format t "don't have that cash")
       (eject-card atm))
      (t
       (format t "~a is provided by the machine" amount)
       (decf (cash atm) amount)
       (eject-card atm)
       (when (zerop (cash atm))
         (setf (state atm) (out-of-cash atm)))))))

(defmethod request-cash ((state no-cash) amount)
  (format t "ATM out of cash"))

;;;; code for proxy pattern

(defclass atm-proxy ()
  ((atm :initform nil)))

(defmethod atm ((proxy atm-proxy))
  (with-slots (atm) proxy
    (unless atm (setf atm (make-instance 'atm)))
    atm))

(defmethod state ((proxy atm-proxy))
  (state (atm proxy)))

(defmethod cash ((proxy atm-proxy))
  (cash (atm proxy)))


;;;; ---------------------------------------------------------------------------

(defparameter *atm* (make-instance 'atm-proxy))
(print (state *atm*))
(print (cash *atm*))
