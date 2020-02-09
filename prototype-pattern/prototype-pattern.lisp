;;;; What is the Prototype Pattern?
;;;;
;;;; - Creating new objects by cloning other objects
;;;; - Allows for adding of any subclass instance of a known super
;;;;   class at run time
;;;; - When there are numerous potential classes that you want to
;;;;   only use if needed at runtime
;;;; - Reduces the need for creating subclasses
;;;;
;;;; @see http://www.newthinktank.com/2012/09/prototype-design-pattern-tutorial/

(defun get-slots (object)
  #+sbcl
  (mapcar #'sb-pcl:slot-definition-name (sb-pcl:class-slots (class-of object)))
  #+clisp
  (mapcar #'clos:slot-definition-name (clos:class-slots (class-of object))))

(defun shallow-copy-object (original)
  (let* ((class (class-of original))
         (copy (allocate-instance class)))
    (dolist (slot (get-slots original))
      (when (slot-boundp original slot)
        (setf (slot-value copy slot)
              (slot-value original slot))))
    copy))

(defgeneric copy (animal))

(defclass sheep ()
  ((name :initarg :name :accessor name)))

(defmethod initialize-instance :after ((instance sheep) &key)
  (format t "sheep is made~%"))

(defmethod copy ((animal sheep))
  (shallow-copy-object animal))

(defclass clone-factory ()
  nil)

(defmethod get-clone ((clone-factory clone-factory) animal)
  (copy animal))


;;;; ---------------------------------------------------------------------------

(let* ((animal-maker (make-instance 'clone-factory))
       (sally (make-instance 'sheep :name "sally"))
       (cloned-sheep (get-clone animal-maker sally)))
  (setf (name cloned-sheep) "sally clone")
  (format t "sally name: ~a~%" (name sally))
  (format t "clone name: ~a~%" (name cloned-sheep)))
