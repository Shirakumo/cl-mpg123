#|
 This file is a part of cl-mpg123
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mpg123)

(defclass metadata ()
  ((version :initform NIL :reader version)
   (fields :initform NIL :reader fields)
   (comments :initform NIL :reader comments)
   (extras :initform NIL :reader extras)
   (pictures :initform NIL :reader pictures)))

(defun mstring (mstring)
  (etypecase mstring
    (foreign-pointer
     (cffi:foreign-string-to-lisp
      (cl-mpg123-cffi:mstring-p mstring)
      :max-chars (cl-mpg123-cffi:mstring-size mstring)
      :encoding :UTF-8))
    (list
     (cffi:foreign-string-to-lisp
      (getf mstring 'cl-mpg123-cffi::p)
      :max-chars (getf mstring 'cl-mpg123-cffi::size)
      :encoding :UTF-8))))

(defmacro do-text-array ((lang id description text) (array size) &body body)
  `(map-text-array (lambda (,lang ,id ,description ,text)
                     ,@body)
                   ,array ,size))

(defun direct-str (pointer length)
  (let ((str (or (ignore-errors (foreign-string-to-lisp pointer :max-chars length :encoding :utf-8))
                 (ignore-errors (foreign-string-to-lisp pointer :max-chars length :encoding :iso-8859-1)))))
    (if (or (not str) (string= "" str)) NIL str)))

(defun map-text-array (func array size)
  (dotimes (i size)
    (let* ((p (mem-aptr array '(:struct cl-mpg123-cffi:text) i))
           (lang (direct-str (cl-mpg123-cffi:text-lang p) 3))
           (id   (direct-str (cl-mpg123-cffi:text-id p) 4))
           (description (mstring (cl-mpg123-cffi:text-description p)))
           (text (mstring (cl-mpg123-cffi:text-text p))))
      (funcall func lang id description text))))

(defmethod initialize-instance :after ((data metadata) &key id3v2 id3v1)
  ;; Fill by id3v1, then override by id3v2.
  (with-slots (version fields comments extras pictures) data
    (flet ((add-field (type text &optional desc)
             (when (and text (not (equal text "")))
               (pushnew (list (intern type :keyword) desc text) fields :test #'equal)))
           (add-comment (text &optional lang desc)
             (when (and text (not (equal text "")))
               (pushnew (list lang desc text) comments :test #'equal))))
      (when id3v1
        (setf version "1.0")
        (add-field "TIT2" (direct-str (cl-mpg123-cffi:id3v1-title id3v1) 30))
        (add-field "TPE1" (direct-str (cl-mpg123-cffi:id3v1-artist id3v1) 30))
        (add-field "TALB" (direct-str (cl-mpg123-cffi:id3v1-album id3v1) 30))
        (add-field "TDRC" (parse-integer (direct-str (cl-mpg123-cffi:id3v1-year id3v1) 4)))
        (add-field "TCON" (id3v1-genre (cl-mpg123-cffi:id3v1-genre id3v1)))
        (let ((cptr (cl-mpg123-cffi:id3v1-comment id3v1))
              (comment NIL))
          ;; id3v1 track number, probably.
          (cond ((= 0 (mem-ref cptr :char 28))
                 (setf version "1.1")
                 (setf comment (direct-str cptr 28))
                 (add-field "TRCK" (mem-ref cptr :char 29)))
                (T
                 (setf comment (direct-str cptr 30))))
          (add-comment comment)))
      (when id3v2
        (setf version (format NIL "2.~a" (cl-mpg123-cffi:id3v2-version id3v2)))
        (do-text-array (lang id desc text)
            ((cl-mpg123-cffi:id3v2-text id3v2)
             (cl-mpg123-cffi:id3v2-texts id3v2))
          (declare (ignore lang))
          ;; Genre gets special processing as it might contain multiple fields in one.
          (cond ((string= id "TCON") (dolist (genre (id3v2-genre text))
                                       (add-field "TCON" genre)))
                (T (add-field id text desc))))
        (do-text-array (lang id desc text)
            ((cl-mpg123-cffi:id3v2-comment-list id3v2)
             (cl-mpg123-cffi:id3v2-comments id3v2))
          (declare (ignore id))
          (add-comment text lang desc))
        (do-text-array (lang id desc text)
            ((cl-mpg123-cffi:id3v2-extra id3v2)
             (cl-mpg123-cffi:id3v2-extras id3v2))
          (declare (ignore lang id))
          (pushnew (list desc text) extras :test #'equal))
        (dotimes (i (cl-mpg123-cffi:id3v2-pictures id3v2))
          (push (make-instance 'picture :struct (mem-aptr (cl-mpg123-cffi:id3v2-picture id3v2) '(:struct cl-mpg123-cffi:picture) i))
                pictures))))))

(defun field (name metadata)
  (let ((name (or (id3v2-type name)
                   (error "Unknown id3v2 frame type ~s." name))))
    (loop for (type desc text) in (fields metadata)
          when (eql name type)
          collect (list desc text))))

(defclass picture ()
  ((kind :reader kind)
   (description :reader description)
   (mime-type :reader mime-type)
   (data :reader data)))

(defmethod initialize-instance :after ((picture picture) &key struct)
  (with-slots (kind description mime-type data) picture
    (setf kind (cl-mpg123-cffi:picture-type struct))
    (setf description (mstring (cl-mpg123-cffi:picture-description struct)))
    (setf mime-type (mstring (cl-mpg123-cffi:picture-mime-type struct)))
    (let ((array (make-array (cl-mpg123-cffi:picture-size struct) :element-type '(unsigned-byte 8)))
          (carray (cl-mpg123-cffi:picture-data struct)))
      (loop for i from 0 below (length array)
            do (setf (aref array i) (mem-aref carray :unsigned-char i)))
      (setf data array))))
