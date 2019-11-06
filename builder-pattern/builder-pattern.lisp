;;;; What is the Builder Pattern?
;;;;
;;;; - Pattern used to create objects made from a bunch of other objects
;;;;   - When you want to build an object made up from other objects
;;;;   - When you want the creation of these parts to be independent
;;;;     of the main object
;;;;   - Hide the creation of the parts from the client so both
;;;;     aren't dependent
;;;;   - The builder know the specifics and nobody else does
;;;;
;;;; @see http://www.newthinktank.com/2012/09/builder-design-pattern-tutorial/

(defclass robot ()
  ((head  :accessor head)
   (torso :accessor torso)
   (arms  :accessor arms)
   (legs  :accessor legs)))

(defclass old-robot-builder ()
  ((robot :initform (make-instance 'robot) :reader robot)))

(defclass robot-engineer ()
  ((robot-builder
    :initform (error "must have a robot builder")
    :initarg :robot-builder
    :reader robot-builder)))

(defgeneric build-robot-head  (robot-builder))
(defgeneric build-robot-torso (robot-builder))
(defgeneric build-robot-arms  (robot-builder))
(defgeneric build-robot-legs  (robot-builder))
(defgeneric robot             (robot-builder))

(defmethod build-robot-head ((robot-builder old-robot-builder))
  (with-slots (robot) robot-builder
    (setf (head robot) "Tin Head")))

(defmethod build-robot-torso ((robot-builder old-robot-builder))
  (with-slots (robot) robot-builder
    (setf (torso robot) "Tin Torso")))

(defmethod build-robot-arms ((robot-builder old-robot-builder))
  (with-slots (robot) robot-builder
    (setf (arms robot) "Blowtorch Arms")))

(defmethod build-robot-legs ((robot-builder old-robot-builder))
  (with-slots (robot) robot-builder
    (setf (legs robot) "Rolle Skates")))

(defmethod robot ((robot-engineer robot-engineer))
  (robot (robot-builder robot-engineer)))

(defmethod make-robot ((robot-maker robot-engineer))
  (with-slots (robot-builder) robot-maker
    (build-robot-head robot-builder)
    (build-robot-torso robot-builder)
    (build-robot-arms robot-builder)
    (build-robot-legs robot-builder)))

;;;; ---------------------------------------------------------------------------

(defparameter *old-style-robot* (make-instance 'old-robot-builder))
(defparameter *robot-engineer* (make-instance 'robot-engineer :robot-builder *old-style-robot*))

(make-robot *robot-engineer*)
(defparameter *first-robot* (robot *robot-engineer*))

(format t "head: ~a~%" (head *first-robot*))
(format t "torso: ~a~%" (torso *first-robot*))
(format t "arms: ~a~%" (arms *first-robot*))
(format t "legs: ~a~%" (legs *first-robot*))
