#|
 This file is a part of cl-mpg123
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mpg123)

(defclass metadata ()
  ((version :initform NIL :reader version)
   (fields :initform NIL :reader fields)
   (pictures :initform NIL :reader pictures)))

(defun map-text-array (func array size)
  (dotimes (i size)
    (let* ((p (mem-aptr array '(:struct cl-mpg123-cffi:text) i))
           (id   (direct-str (cl-mpg123-cffi:text-id p) 4))
           (lang (direct-str (cl-mpg123-cffi:text-lang p) 3))
           (description (mstring (cl-mpg123-cffi:text-description p)))
           (text (mstring (cl-mpg123-cffi:text-text p))))
      (funcall func lang id description text))))

(defmacro do-text-array ((lang id description text) (array size) &body body)
  `(map-text-array (lambda (,lang ,id ,description ,text)
                     ,@body)
                   ,array ,size))

(defmethod shared-initialize :after ((data metadata) slots &key id3v2 id3v1 (id3v1-encoding :utf-8))
  ;; Fill by id3v1, then override by id3v2.
  (with-slots (version fields pictures) data
    (labels ((add-field (type text &optional lang desc)
               (etypecase text
                 (null)
                 (list (dolist (tex text) (add-field type tex lang desc)))
                 (T (pushnew (list (intern type :keyword) lang desc text)
                             fields :test #'equal))))
             (str (pointer length)
               (direct-str pointer length id3v1-encoding)))
      (when id3v1
        (setf version "1.0")
        (add-field "TIT2" (str (cl-mpg123-cffi:id3v1-title id3v1) 30))
        (add-field "TPE1" (str (cl-mpg123-cffi:id3v1-artist id3v1) 30))
        (add-field "TALB" (str (cl-mpg123-cffi:id3v1-album id3v1) 30))
        (add-field "TRCK" (parse-integer (str (cl-mpg123-cffi:id3v1-year id3v1) 4)))
        (add-field "TCON" (id3v1-genre (cl-mpg123-cffi:id3v1-genre id3v1)))
        (let ((cptr (cl-mpg123-cffi:id3v1-comment id3v1))
              (comment NIL))
          ;; id3v1 track number, probably.
          (cond ((= 0 (mem-ref cptr :char 28))
                 (setf version "1.1")
                 (setf comment (str cptr 28))
                 (add-field "TRCK" (mem-ref cptr :char 29)))
                (T
                 (setf comment (str cptr 30))))
          (add-field "COMM" comment)))
      (when id3v2
        (setf version (format NIL "2.~a" (cl-mpg123-cffi:id3v2-version id3v2)))
        (do-text-array (lang id desc text)
            ((cl-mpg123-cffi:id3v2-text id3v2)
             (cl-mpg123-cffi:id3v2-texts id3v2))
          (cond ((find id '("TCOM" "TEXT" "TOLY" "TOPE" "TPE1") :test #'string=) (add-field id (split text #\/) lang desc))
                ((find id '("TORY" "TYER") :test #'string=) (add-field id (parse-integer text) lang desc))
                ((string= id "TRCK") (add-field id (parse-integer
                                                    (let ((slashpos (position #\/ text)))
                                                      (if slashpos (subseq text 0 slashpos) text)))
                                                lang desc))
                ((string= id "TCON") (add-field id (id3v2-genre text) lang desc))
                (T (add-field id text lang desc))))
        (do-text-array (lang id desc text)
            ((cl-mpg123-cffi:id3v2-comment-list id3v2)
             (cl-mpg123-cffi:id3v2-comments id3v2))
          (add-field id text lang desc))
        (do-text-array (lang id desc text)
            ((cl-mpg123-cffi:id3v2-extra id3v2)
             (cl-mpg123-cffi:id3v2-extras id3v2))
          (add-field id text lang desc))
        (dotimes (i (cl-mpg123-cffi:id3v2-pictures id3v2))
          (push (make-instance 'picture :struct (mem-aptr (cl-mpg123-cffi:id3v2-picture id3v2) '(:struct cl-mpg123-cffi:picture) i))
                pictures))))))

(defmethod reinitialize-instance :before ((data metadata) &key)
  (with-slots (fields pictures) data
    (setf fields ())
    (setf pictures ())))

(defun field (name metadata)
  (let ((name (or (id3v2-type name)
                  (error 'unknown-id3v2-frame-type :name name))))
    (loop for (type lang desc text) in (fields metadata)
          when (eql name type)
          collect (list lang desc text))))

(defun field-text (name metadata)
  (values-list (mapcar #'third (field name metadata))))

(defclass picture ()
  ((kind :reader kind)
   (description :reader description)
   (mime-type :reader mime-type)
   (data :reader data)))

(defmethod shared-initialize :after ((picture picture) slots &key struct)
  (when struct
    (with-slots (kind description mime-type data) picture
      (setf kind (cl-mpg123-cffi:picture-type struct))
      (setf description (mstring (cl-mpg123-cffi:picture-description struct)))
      (setf mime-type (mstring (cl-mpg123-cffi:picture-mime-type struct)))
      (let ((array (make-array (cl-mpg123-cffi:picture-size struct) :element-type '(unsigned-byte 8)))
            (carray (cl-mpg123-cffi:picture-data struct)))
        (loop for i from 0 below (length array)
              do (setf (aref array i) (mem-aref carray :unsigned-char i)))
        (setf data array)))))
