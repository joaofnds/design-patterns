;;;; What is the Adapter Pattern?
;;;;
;;;; - Allows two incomplatible interfaces to work togheter
;;;; - Used when the client expects a (target) interface
;;;; - The adapter class allows the use of the available interface and
;;;;   the Target interface
;;;; - Any class can work togheter as long as the Adapter solves the
;;;;   issue that all classes must implement every method by the shared interface
;;;;
;;;; @see http://www.newthinktank.com/2012/09/adapter-design-pattern-tutorial/

(defgeneric fire-weapon (enemy-attacker))
(defgeneric drive-forward (enemy-attacker))
(defgeneric assign-driver (enemy-attacker driver-name))

;; target
(defclass enemy-tank () nil)

(defmethod fire-weapon ((enemy-attacker enemy-tank))
  (format t "Enemy tank does ~a damage~%" 10))
(defmethod drive-forward ((enemy-attacker enemy-tank))
  (format t "enemy tank moves ~a spaces~%" 10))
(defmethod assign-driver ((enemy-attacker enemy-tank) driver-name)
  (format t "~a is driving the tank~%" driver-name))

;; adaptee
(defclass enemy-robot () nil)

(defmethod smash-with-hands (enemy-robot)
  (format t "Enemy robot causes ~a damage with its hands~%" 10))
(defmethod walk-forward (enemy-robot)
  (format t "Enemy robot walks forward ~a spaces~%" 10))
(defmethod react-to-human (enemy-robot human-name)
  (format t "Enemy robot tramps on ~a~%" human-name))

(defclass enemy-robot-adapter ()
  ((enemy-robot :initarg :enemy-robot
                :initform (error "must have enemy-robot")
                :reader enemy-robot)))

(defmethod fire-weapon ((enemy-attacker enemy-robot-adapter))
  (smash-with-hands (enemy-robot enemy-attacker)))
(defmethod drive-forward ((enemy-attacker enemy-robot-adapter))
  (walk-forward (enemy-robot enemy-attacker)))
(defmethod assign-driver ((enemy-attacker enemy-robot-adapter) driver-name)
  (react-to-human (enemy-robot enemy-attacker) driver-name))

;; -----------------------------------------------------------------------------

(defvar *rx7-tank* (make-instance 'enemy-tank))
(defvar *fred-the-robot* (make-instance 'enemy-robot))
(defvar *robot-adapter* (make-instance 'enemy-robot-adapter :enemy-robot *fred-the-robot*))

(format t "The robot~%")
(react-to-human *fred-the-robot* "João")
(walk-forward *fred-the-robot*)
(smash-with-hands *fred-the-robot*)

(format t "~%The enemy tank~%")
(assign-driver *rx7-tank* "João")
(drive-forward *rx7-tank*)
(fire-weapon *rx7-tank*)

(format t "~%The robot with adapter~%")
(assign-driver *robot-adapter* "João")
(drive-forward *robot-adapter*)
(fire-weapon *robot-adapter*)
