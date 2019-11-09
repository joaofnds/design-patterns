;;;; What is the Singleton Pattern?
;;;;
;;;; It is used when you want to eliminate the option of instantiating
;;;; more than one object
;;;;
;;;; @see http://www.newthinktank.com/2012/09/singleton-design-pattern-tutorial/

(defclass singleton ()
  ((instance :initform nil :accessor instance :allocation :class)))

;; because class slots are only accessible through an instance, we need to instantiate
;; it first, so we can get hold of that slot
(defmethod make-instance ((class (eql 'singleton)) &key)
  (let ((obj (call-next-method)))
    (with-slots (instance) obj
      (unless instance
        (setf instance obj))
      instance)))

;;;; ---------------------------------------------------------------------------

(print (make-instance 'singleton))
(print (make-instance 'singleton))
