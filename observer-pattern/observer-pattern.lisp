;; frozen_string_literal: true
;;
;; When to use the Observer Pattern
;;
;; - when you need many other object to receive an update when
;;   another object changes
;; Ex:
;;   - Stock market with thousands of stocks need to send updates to
;;     objects representing individual stocks
;;   - The Subject(publisher) sends many stocks to the observers
;;   - The Observers(subscribers) takes the ones they want and use them
;;
;; @see http://www.newthinktank.com/2012/08/observer-design-pattern-tutorial/

(defgeneric register (subject observer))
(defgeneric unregister (subject observer))
(defgeneric notify-observers (subject))

(defgeneric update (observer stocks))

(defgeneric find-stock (grabber name))
(defgeneric add-stock (grabber name price))
(defgeneric update-stock (grabber name price))
(defgeneric remove-stock (grabber name))

(defclass stock-grabber ()
  ((observers
    :initform '()
    :accessor observers)
   (stocks
    :initform '()
    :accessor stocks)))

(defmethod register ((subject stock-grabber) observer)
  (push observer (observers subject)))

(defmethod unregister ((subject stock-grabber) observer)
  (delete observer (observers subject)))

(defmethod notify-observers ((subject stock-grabber))
  (dolist (observer (observers subject))
    (update observer (stocks subject))))

(defmethod find-stock ((grabber stock-grabber) name)
  (assoc name (stocks grabber) :test #'string-equal))

(defmethod add-stock ((grabber stock-grabber) name price)
  (unless (find-stock grabber name)
    (push (cons name price) (stocks grabber))))

(defmethod add-stock :after ((grabber stock-grabber) name price)
  (notify-observers grabber))


(defmethod update-stock ((grabber stock-grabber) name price)
  (setf (cdr (find-stock grabber name)) price))

(defmethod update-stock :after ((grabber stock-grabber) name price)
  (notify-observers grabber))


(defmethod remove-stock ((grabber stock-grabber) name)
  (setf (stocks grabber)
        (remove name (stocks grabber) :test #'string-equal :key #'car)))

(defmethod remove-stock :after ((grabber stock-grabber) name)
  (notify-observers grabber))

(defparameter *next-observer-id* 1)

(defclass stock-observer ()
  ((id
    :initform *next-observer-id*
    :reader id)
   (stocks
    :initform '()
    :accessor stocks)
   (grabber
    :initform (error "must have grabber.")
    :initarg :grabber
    :reader grabber)))

(defmethod initialize-instance :after ((observer stock-observer) &key grabber)
  (register grabber observer)
  (incf *next-observer-id*))

(defmethod update ((observer stock-observer) stocks)
  (format t "[observer ~a] stocks updated!~%" (id observer))
  (loop :for (name . price) :in stocks
        :do (format t "~a: ~a~%" name price))
  (setf (stocks observer) stocks))

;; ------------------------------------------------------------------------------

(defparameter grabber (make-instance 'stock-grabber))

(defparameter observer-1 (make-instance 'stock-observer :grabber grabber))
(defparameter observer-2 (make-instance 'stock-observer :grabber grabber))

(add-stock grabber "GOOG" 10)
(add-stock grabber "AAPL" 20)
(add-stock grabber "AMZN" 30)

(update-stock grabber "AMZN" 40)

(remove-stock grabber "AAPL")
