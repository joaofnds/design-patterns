;;;; What is the Abstract Factory Pattern?
;;;;
;;;; - It is like a factory, but everything is encapsulated
;;;;   - The method that orders the subject
;;;;   - The factories that build the object
;;;;   - The final objects
;;;;   - The final objects contain objects that use Strategy Pattern
;;;;     - Composition: Object class fields are objects
;;;;
;;;; @see http://www.newthinktank.com/2012/09/abstract-factory-design-pattern/

(defclass enemy-ship ()
  ((name   :initarg :name   :accessor name)
   (weapon :initarg :weapon :accessor weapon)
   (engine :initarg :engine :accessor engine)))

(defclass ufo-enemy-ship (enemy-ship)
  ((factory :initarg :factory :reader factory)))

(defclass ufo-boss-enemy-ship (enemy-ship)
  ((factory :initarg :factory :reader factory)))

(defclass enemy-ship-gun ()
  ((damage
    :initform (error "must have damage")
    :initarg :damage
    :reader damage)))

(defclass enemy-ship-engine ()
  ((engine-speed
    :initform (error "must have engine-speed")
    :initarg :engine-speed
    :reader engine-speed)))

(defclass enemy-ship-ufo-gun (enemy-ship-gun)
  ((damage :initform 10)))

(defclass enemy-ship-ufo-engine (enemy-ship-engine)
  ((engine-speed :initform 1000)))

(defclass enemy-ship-ufo-boss-gun (enemy-ship-gun)
  ((damage :initform 40)))

(defclass enemy-ship-ufo-boss-engine (enemy-ship-engine)
  ((engine-speed :initform 2000)))

(defclass enemy-ship-building () ())

(defclass ufo-enemy-ship-building (enemy-ship-building) ())

(defclass ufo-enemy-ship-factory () ())
(defclass ufo-boss-enemy-ship-factory () ())

(defmethod print-object ((object enemy-ship-engine) stream)
  (format stream "~d km/h" (engine-speed object)))

(defmethod print-object ((object enemy-ship-gun) stream)
  (format stream "~d" (damage object)))

(defgeneric order-the-ship (ship-builder type))
(defmethod order-the-ship ((ship-builder enemy-ship-building) type)
  (let ((ship (make-enemy-ship ship-builder type)))
    (make-ship ship)
    (display-enemy-ship ship)
    (follow-hero-ship ship)
    (enemy-ship-shoots ship)
    ship))

(defgeneric make-enemy-ship (ship-builder type))
(defmethod make-enemy-ship ((ship-builder enemy-ship-building) type)
  (let ((ship nil))
    (ecase type
      ('ufo
       (let ((ship-parts-factory (make-instance 'ufo-enemy-ship-factory)))
         (setf ship (make-instance 'ufo-enemy-ship :factory ship-parts-factory))
         (setf (name ship) "UFO Grunt Ship")))
      ('ufo-boss
       (let ((ship-parts-factory (make-instance 'ufo-boss-enemy-ship-factory)))
         (setf ship (make-instance 'ufo-enemy-ship :factory ship-parts-factory))
         (setf (name ship) "UFO Boss Ship"))))
    ship))

(defgeneric add-enemy-ship-gun (factory))
(defmethod add-enemy-ship-gun ((factory ufo-enemy-ship-factory))
  (make-instance 'enemy-ship-ufo-gun))
(defmethod add-enemy-ship-gun ((factory ufo-boss-enemy-ship-factory))
  (make-instance 'enemy-ship-ufo-boss-gun))

(defgeneric add-enemy-ship-engine (factory))
(defmethod add-enemy-ship-engine ((factory ufo-enemy-ship-factory))
  (make-instance 'enemy-ship-ufo-engine))
(defmethod add-enemy-ship-engine ((factory ufo-boss-enemy-ship-factory))
  (make-instance 'enemy-ship-ufo-boss-engine))

(defgeneric follow-hero-ship (enemy-ship))
(defmethod follow-hero-ship ((enemy-ship enemy-ship))
  (with-slots (name engine) enemy-ship
    (format t "~a is following the hero at ~a~%" name engine)))

(defgeneric display-enemy-ship (enemy-ship))
(defmethod display-enemy-ship ((enemy-ship enemy-ship))
  (format t "~a is on the screen~%" (name enemy-ship)))

(defgeneric enemy-ship-shoots (enemy-ship))
(defmethod enemy-ship-shoots ((enemy-ship enemy-ship))
  (with-slots (name weapon) enemy-ship
    (format t "~a attack and does ~a~%" name weapon)))

(defgeneric make-ship (enemy-ship))
(defmethod make-ship ((enemy-ship ufo-enemy-ship))
  (with-slots (factory name weapon engine) enemy-ship
    (format t "Making enemy ship ~a~%" name)
    (setf weapon (add-enemy-ship-gun factory))
    (setf engine (add-enemy-ship-engine factory))))
(defmethod make-ship ((enemy-ship ufo-boss-enemy-ship))
  (with-slots (factory name weapon engine) enemy-ship
    (format t "Making enemy ship ~a~%" name)
    (setf weapon (add-enemy-ship-gun factory))
    (setf engine (add-enemy-ship-engine factory))))

;;;; ---------------------------------------------------------------------------

(defvar *make-ufos* (make-instance 'ufo-enemy-ship-building))
(defvar *the-grunt* (order-the-ship *make-ufos* 'ufo))
(defvar *the-boss* (order-the-ship *make-ufos* 'ufo-boss))
