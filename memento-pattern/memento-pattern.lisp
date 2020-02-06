;;;; What is the Memento Design Pattern?
;;;; - Used to store an object state at a point in time so it can be
;;;;   returned to that state later.
;;;; - It simply allows you to undo/redo changes on an Object
;;;; - Memento: The basic object that is stored in different states
;;;; - Originator: Sets and Gets values from the currently targeted
;;;;   Memento. Create new Mementos and assigns current values to them
;;;; - Caretaker: Holds a List that contains all previous versions of the Memento.
;;;;   It can store and retrieve stored Mementos
;;;;
;;;; @see http://www.newthinktank.com/2012/10/memento-design-pattern-tutorial/

(defclass memento ()
  ((state :initarg :state :initform (error "must have state") :reader state)))

(defmethod initialize-instance :after ((instance memento) &key state)
  (setf (slot-value instance 'state)
        (copy-seq state)))


(defclass originator ()
  ((state :accessor state)))

(defmethod create-memento ((originator originator))
  (with-slots (state) originator
    (make-instance 'memento :state state)))

(defmethod set-memento ((originator originator) (memento memento))
  (with-slots (state) originator
    (setf state (state memento))))


(defclass care-taker ()
  ((states :initform nil :reader states)))

(defmethod add-memento ((care-taker care-taker) (memento memento))
  (with-slots (states) care-taker
    (setf states (append states (cons memento nil)))))

(defmethod get-memento ((care-taker care-taker) index)
  (elt (states care-taker) index))

;;;; ---------------------------------------------------------------------------

(defclass editor ()
  ((text
    :initarg :text
    :initform (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)
    :accessor text)
   (originator
    :initarg :originator
    :initform (make-instance 'originator)
    :reader originator)
   (care-taker
    :initarg :care-taker
    :initform (make-instance 'care-taker)
    :reader care-taker)
   (versions
    :initarg :versions
    :initform 0
    :accessor versions)
   (current-version
    :initarg :current-version
    :initform 0
    :accessor current-version)))

(defmethod save ((editor editor))
  (with-slots (text versions current-version originator care-taker) editor
    (setf (state originator) text)
    (add-memento care-taker (create-memento originator))
    (incf versions)
    (incf current-version)))

(defmethod undo ((editor editor))
  (with-slots (text current-version care-taker originator) editor
    (when (> current-version 0)
      (decf current-version)
      (set-memento originator (get-memento care-taker current-version))
      (setf text (state originator)))))

(defmethod redo ((editor editor))
  nil)


(defparameter *editor* (make-instance 'editor))

(dolist (word '("foo" "bar" "baz"))
  (setf (text *editor*) word)
  (save *editor*))

(undo *editor*)
(format t "~a~%" (text *editor*)) ;; => baz
(undo *editor*)
(format t "~a~%" (text *editor*)) ;; => bar
(undo *editor*)
(format t "~a~%" (text *editor*)) ;; => foo
