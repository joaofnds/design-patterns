;;;; What is the Bridge Pattern?
;;;;
;;;; - Decouple an abstraction from its implementation so that the
;;;;   two can vary independently
;;;; - The bridge pattern is very poorly explained, everyone seems
;;;;   to explain it differenlty
;;;; - Progessively adding functionality while separating out major
;;;;   differences using abstract class
;;;;
;;;; @see http://www.newthinktank.com/2012/10/bridge-design-pattern-tutorial/

(defclass entertainment-device ()
  ((state       :reader state)
   (max-setting :reader max-setting)
   (volume      :reader volume :initform 0)))

(defclass tv-device (entertainment-device)
  ((state :initform (error "must have an initial state")
          :initarg :state)
   (max-setting :initform (error "must have an initial max-setting")
                :initarg :max-setting)))

(defclass remote-button ()
  ((device :initform (error "must have a device")
           :initarg :device
           :reader device)))

(defclass tv-remote-mute (remote-button) ())
(defmethod button-nine-pressed ((device tv-remote-mute))
  (format t "tv was muted~%"))

(defclass tv-remote-pause (remote-button) ())
(defmethod button-nine-pressed ((device tv-remote-pause))
  (format t "tv was paused~%"))

(defgeneric button-five-pressd (device))
(defmethod button-five-pressed ((device entertainment-device)))
(defmethod button-five-pressed ((device tv-device))
  (format t "channel down~%")
  (decf (slot-value device 'state)))
(defmethod button-five-pressed ((device remote-button))
  (button-five-pressed (device device)))

(defgeneric button-six-pressed (device))
(defmethod button-six-pressed ((device entertainment-device))
  (format t "channel up~%")
  (incf (slot-value device 'state)))
(defmethod button-six-pressed ((device remote-button))
  (button-six-pressed (device device)))

(defgeneric button-seven-pressed (device))
(defmethod button-seven-pressed ((device entertainment-device))
  (with-slots (volume) device
    (incf volume)
    (format t "volume at: ~a~%" volume)))
(defmethod button-seven-pressed ((device remote-button))
  (button-seven-pressed (device device)))

(defgeneric device-feedback (device))
(defmethod device-feedback ((device entertainment-device))
  (with-slots (state max-setting) device
    (when (not (< 0 state max-setting))
      (setf state 0))
    (format t "On: ~a~%" state)))
(defmethod device-feedback ((device remote-button))
  (device-feedback (device device)))

;;;; ---------------------------------------------------------------------------

(defparameter *device-1* (make-instance 'tv-device :state 1 :max-setting 10))
(defparameter *device-2* (make-instance 'tv-device :state 1 :max-setting 10))
(defparameter *tv-1* (make-instance 'tv-remote-mute :device *device-1*))
(defparameter *tv-2* (make-instance 'tv-remote-pause :device *device-2*))

(format t "test tv with mute~%")
(button-five-pressed *tv-1*)
(button-six-pressed *tv-1*)
(button-seven-pressed *tv-1*)
(button-nine-pressed *tv-1*)

(format t "~%test tv with pause~%")
(button-five-pressed *tv-2*)
(button-six-pressed *tv-2*)
(button-nine-pressed *tv-2*)
