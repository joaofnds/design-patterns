;;;; What is the Iterator Design Pattern?
;;;;
;;;; - The iterator pattern provides you with a uniform way to access
;;;;   different collections of Object
;;;; - If you get an Array, ArrayList and Hashtable of Object,
;;;;   you pop out an iterator for each and treat them the same
;;;; - This provides a uniform way to cycle through different collections
;;;; - You can also write polymorphic code because you can refer to
;;;;   each collection of objects because they'll implement the same interface
;;;;
;;;; @see http://www.newthinktank.com/2012/10/iterator-design-pattern-tutorial/

(defclass song ()
  ((name         :reader name         :initarg :name)
   (band         :reader band         :initarg :band)
   (release-year :reader release-year :initarg :release-year)))

(defgeneric add-song (playlist name band release-year))

(defclass songs-of-the-70s ()
  ((songs :initform '() :accessor songs)))

(defmethod initialize-instance :after ((class songs-of-the-70s) &key)
  (add-song class "Imagine" "John Lennon" 1971)
  (add-song class "American Pie" "Don McLean" 1971)
  (add-song class "I Will Survive" "Gloria Gaynor" 1978))

(defmethod add-song ((playlist songs-of-the-70s) name band release-year)
  (push
   (make-instance 'song :name name :band band :release-year release-year)
   (songs playlist)))


(defclass songs-of-the-80s ()
  ((songs :initform '() :accessor songs)))

(defmethod initialize-instance :after ((class songs-of-the-80s) &key)
  (add-song class "Roam" "B 52s" 1989)
  (add-song class "Cruel Summer" "Bananarama" 1984)
  (add-song class "Head Over Heels" "Tears For Fears" 1985))

(defmethod add-song ((playlist songs-of-the-80s) name band release-year)
  (pushnew
   (make-instance 'song :name name :band band :release-year release-year)
   (songs playlist)
   :test #'equal))


(defclass songs-of-the-90s ()
  ((songs    :initform (make-hash-table))
   (hash-key :accessor hash-key :initform 0)))

(defmethod initialize-instance :after ((class songs-of-the-90s) &key)
  (add-song class "Losing My Religion" "REM" 1991)
  (add-song class "Creep" "Radiohead" 1993)
  (add-song class "Walk on the Ocean" "Toad The Wet Sprocket" 1991))

(defmethod add-song ((playlist songs-of-the-90s) name band release-year)
  (with-slots (songs hash-key) playlist
    (setf (gethash hash-key songs)
          (make-instance 'song :name name :band band :release-year release-year))))

(defmethod songs ((playlist songs-of-the-90s))
  (with-slots (songs) playlist
    (loop :for song :being :the :hash-values :in songs
          :collect song)))

(defmethod add-song :after ((playlist songs-of-the-90s) name band release-year)
  (declare (ignore name band release-year))
  (incf (hash-key playlist)))


(defclass disk-jockey ()
  ((songs-of-the-70s :reader songs-of-the-70s :initform (make-instance 'songs-of-the-70s))
   (songs-of-the-80s :reader songs-of-the-80s :initform (make-instance 'songs-of-the-80s))
   (songs-of-the-90s :reader songs-of-the-90s :initform (make-instance 'songs-of-the-90s))))

(defmethod show-the-songs ((disk-jockey disk-jockey))
  (with-slots (songs-of-the-70s songs-of-the-80s songs-of-the-90s) disk-jockey
    (format t "Songs of the 70s~%")
    (show-decade-songs (songs songs-of-the-70s))
    (format t "Songs of the 80s~%")
    (show-decade-songs (songs songs-of-the-80s))
    (format t "Songs of the 90s~%")
    (show-decade-songs (songs songs-of-the-90s))))

(defun show-decade-songs (songs)
  (dolist (song songs)
    (format t "  Name:         ~a~%" (name song))
    (format t "  Band:         ~a~%" (band song))
    (format t "  Release year: ~a~2%" (release-year song))))

;;;; ---------------------------------------------------------------------------

(defparameter *disk-jockey* (make-instance 'disk-jockey))
(show-the-songs *disk-jockey*)
