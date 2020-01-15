;;;; What is the Composite Design Pattern?
;;;; - Allows you to treat individual objects and
;;;;   compositions of objects uniformly
;;;; - They allow you to represent part-whole hierarchies
;;;; - Components can be further divided into smaller components
;;;; - You can structure data or represent the inner working of
;;;;   every part of a whole object individually
;;;;
;;;; @see http://www.newthinktank.com/2012/10/composite-design-pattern-tutorial/

(defgeneric add (obj song-component))
(defgeneric remove-component (obj song-component))
(defgeneric component (obj index))
(defgeneric song-name (obj))
(defgeneric band-name (obj))
(defgeneric release-year (obj))
(defgeneric info (obj))

(defclass song-group ()
  ((name        :reader name        :initarg :name)
   (description :reader description :initarg :description)
   (songs       :accessor songs       :initform '())))

(defmethod add ((obj song-group) song-component)
  (push song-component (songs obj)))

(defmethod remove-component ((obj song-group) song-component)
  (remove song (songs obj)))

(defmethod get-component ((obj song-group) index)
  (elt (songs obj) index))

(defmethod info ((obj song-group))
  (with-output-to-string (info)
    (format info "~a~%" (name obj))
    (format info "~a~%" (description obj))
    (format info "~{~a~%~}" (map 'list #'info (songs obj)))))

(defclass song ()
  ((name         :reader name         :initarg :name)
   (band         :reader name         :initarg :band)
   (release-year :reader release-year :initarg :release-year)))

(defmethod info ((obj song))
  (with-output-to-string (info)
    (with-slots (name band release-year) obj
        (format info "~a was recorded by ~a in ~a" name band release-year))))

;;;; ---------------------------------------------------------------------------

(defparameter *industrial* (make-instance 'song-group :name "Industrial Music" :description "is a style of experimental music that draws on transgressive and provocative themes"))
(defparameter *heavy-metal* (make-instance 'song-group :name "Heavy Metal" :description "is a genre of rock that developed in the late 1960s, largely in the UK and in the US"))
(defparameter *dubstep* (make-instance 'song-group :name "DubStep" :description "is a genre of electronic dance music that originated in South London, England"))
(defparameter *every-song* (make-instance 'song-group :name "Song List" :description "Every Song Available"))

(add *industrial* *dubstep*)

(add *industrial* (make-instance 'song :name "Head Like a Hole" :band "NIN" :release-year 1990))
(add *industrial* (make-instance 'song :name "Headhunter" :band "Front 242" :release-year 1988))

(add *dubstep* (make-instance 'song :name "Centipede" :band "Knife Party" :release-year 2012))
(add *dubstep* (make-instance 'song :name "Tetris" :band "Doctor P" :release-year 2011))

(add *heavy-metal* (make-instance 'song :name "War Pigs" :band "Black Sabath" :release-year 1970))
(add *heavy-metal* (make-instance 'song :name "Ace of Spades" :band "Motorhead" :release-year 1980))

(add *every-song* *industrial*)
(add *every-song* *heavy-metal*)

(format t "~a~%" (info *every-song*))
