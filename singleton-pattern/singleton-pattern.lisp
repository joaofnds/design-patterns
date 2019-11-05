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
(defun singleton-instance (class-name)
  (let ((obj (make-instance class-name)))
    (with-slots (instance) obj
      (unless instance
        (setf instance obj))
      instance)))

;;;; ---------------------------------------------------------------------------

(print (singleton-instance 'singleton))
(print (singleton-instance 'singleton))
