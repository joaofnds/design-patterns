;;;;  What is the Command Design Pattern?
;;;;
;;;;  - The command pattern is a behavioural design pattern in which an object is
;;;;    used to represent and encapsulate all the information needed to call
;;;;    a method at a later time
;;;;  - This information includes the method name, the object that owns the method
;;;;    and values for the method parameters
;;;;  - Allows you to store lists of code that is used at a later time
;;;;    or many times
;;;;  - Client says "I want a specific Command to run when execute() is called on
;;;;    one of these encapsulated(hidden) Objects"
;;;;  - An Object called the Invoker transfers this Command to another Object
;;;;    called a Receiver to execute the right code
;;;;
;;;;  @see http://www.newthinktank.com/2012/09/command-design-pattern-tutorial/

(defgeneric execute (command))
(defgeneric on (device))
(defgeneric off (device))
(defgeneric volume-up (device))
(defgeneric volume-down (device))

(defclass television ()
  ((volume :initform 0 :accessor volume)))

(defmethod on ((device television))
  (format t "TV is ON~%"))

(defmethod off ((device television))
  (format t "TV is OFF~%"))

(defmethod volume-up ((device television))
  (with-slots (volume) device
    (incf volume)
    (format t "TV Volume it at ~a~%" volume)))

(defmethod volume-down ((device television))
  (with-slots (volume) device
    (decf volume)
    (format t "TV Volume it at ~a~%" volume)))

(defclass turn-tv-on ()
  ((device :initform (error "must have a device") :initarg :device :reader device)))

(defmethod execute ((command turn-tv-on))
  (on (device command)))

(defclass turn-tv-off ()
  ((device :initform (error "must have a device") :initarg :device :reader device)))

(defmethod execute ((command turn-tv-off))
  (off (device command)))

(defclass turn-tv-up ()
  ((device :initform (error "must have a device") :initarg :device :reader device)))

(defmethod execute ((command turn-tv-up))
  (volume-up (device command)))

(defclass turn-tv-down ()
  ((device :initform (error "must have a device") :initarg :device :reader device)))

(defmethod execute ((command turn-tv-down))
  (volume-down (device command)))

(defclass device-button ()
  ((command :initform (error "must have a command") :initarg :command :reader command)))

(defmethod press (device-button)
  (execute (command device-button)))

;; -----------------------------------------------------------------------------

(let* ((tv (make-instance 'television))
       (on-command (make-instance 'turn-tv-on :device tv))
       (off-command (make-instance 'turn-tv-off :device tv))
       (up-command (make-instance 'turn-tv-up :device tv))
       (down-command (make-instance 'turn-tv-down :device tv))
       (on-button (make-instance 'device-button :command on-command))
       (off-button (make-instance 'device-button :command off-command))
       (up-button (make-instance 'device-button :command up-command))
       (down-button (make-instance 'device-button :command down-command)))
  (press on-button)
  (press up-button)
  (press up-button)
  (press up-button)
  (press down-button)
  (press off-button))
