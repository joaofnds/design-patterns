;; What is the Factory Pattern?
;;
;; - When a method return one of several possible classes that
;;   share a common super class
;; Ex:
;;   - Create a new enemy in a game
;;   - Random number generator picks a number assigned to a specific enemy
;;   - The factory returns the enemy associated with that number
;; - The class is chosen at run time
;;
;; @see http://www.newthinktank.com/2012/09/factory-design-pattern-tutorial/

(defclass enemy-ship ()
  ((name
    :initform (error "must have name")
    :initarg :name
    :accessor name)
   (damage
    :initform (error "must have damage")
    :initarg :damage
    :accessor damage)))

(defgeneric follow-hero-ship (ship))
(defgeneric display-enemy-ship (ship))
(defgeneric enemy-ship-shoots (ship))

(defmethod follow-hero-ship ((ship enemy-ship))
  (format t "~a if following the hero~%" (name ship)))

(defmethod display-enemy-ship ((ship enemy-ship))
  (format t "~a is on the screen~%" (name ship)))

(defmethod enemy-ship-shoots ((ship enemy-ship))
  (format t "~a attacks and does ~a~%" (name ship) (damage ship)))

(defclass ufo-enemy-ship (enemy-ship)
  ((name :initform "UFO Enemy Ship")
   (damage :initform 20)))

(defclass rocket-enemy-ship (enemy-ship)
  ((name :initform "Rocket Enemy Ship")
   (damage :initform 10)))

(defclass big-ufo-enemy-ship (enemy-ship)
  ((name :initform "Big UFO Enemy Ship")
   (damage :initform 40)))

(defclass enemy-ship-factory () ())

(defgeneric make-ship (factory type))
(defmethod make-ship ((factory enemy-ship-factory) type)
  (ecase type
    (ufo (make-instance 'ufo-enemy-ship))
    (rocket (make-instance 'rocket-enemy-ship))
    (big-ufo (make-instance 'big-ufo-enemy-ship))))

;; -----------------------------------------------------------------------------

(defun exercise-ship (ship)
  (display-enemy-ship ship)
  (follow-hero-ship ship)
  (enemy-ship-shoots ship))

(let ((factory (make-instance 'enemy-ship-factory)))
  (exercise-ship (make-ship factory 'big-ufo)))
