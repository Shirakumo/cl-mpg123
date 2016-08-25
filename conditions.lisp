#|
 This file is a part of cl-mpg123
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mpg123)

(defmacro define-simple-condition (name superclasses &body report)
  `(define-condition ,name ,superclasses
     ,(when (listp (first report)) (first report))
     (:report (lambda (c s) (format s ,@(if (listp (first report))
                                            (cdr report) report))))))

(defmacro define-direct-condition (name superclasses format-string &rest args)
  `(define-simple-condition ,name ,superclasses
     ,(loop for arg in args
            when (and (listp arg) (eql 'c (second arg)) (not (eql (first arg) 'error-string)))
            collect `(,(first arg) :initarg ,(intern (string (first arg)) :keyword)
                                   :initform NIL
                                   :reader ,(first arg)))
     ,format-string ,@args))

(define-condition mpg-error (error)
  ())

(define-direct-condition unknown-id3v2-frame-type (mpg-error)
  "Unknown id3v2 frame type ~s." (name c))

(define-condition error-string-error (mpg-error)
  ((error :initarg :error :initform NIL :reader error-code)))

(defmethod error-string ((error error-string-error))
  (cl-mpg123-cffi:plain-strerror (error-code error)))

(define-direct-condition generic-error (error-string-error)
  "Failed to execute ~s~@[: ~a~]" (form c) (error-string c))

(define-simple-condition init-failed (error-string-error)
  "Failed to initialise libmpg123: ~a" (error-string c))

(define-condition mpg-file-error (mpg-error)
  ((file :initarg :file :reader file)))

(define-condition mpg-file-string-error (error-string-error mpg-file-error)
  ())

(define-simple-condition handler-creation-failed (mpg-file-string-error)
  "Failed to create file handle for ~a: ~a" (file c) (error-string c))

(define-simple-condition not-connected (mpg-file-error)
  "Attempted to perform an operation that requires ~a to be connected." (file c))

(define-direct-condition connection-failed (mpg-file-string-error)
  "Failed to connect ~a to file ~a: ~a" (file c) (path c) (error-string c))

(define-simple-condition disconnection-failed (mpg-file-string-error)
  "Failed to disconnect ~a: ~a" (file c) (error-string c))

(define-direct-condition decoder-set-failed (mpg-file-string-error)
  "Failed to set decoder to ~s on ~a: ~a" (value c) (file c) (error-string c))

(define-simple-condition read-failed (mpg-file-string-error)
  ((buffer :initarg :buffer :reader buffer)
   (buffer-size :initarg :buffer-size :reader buffer-size))
  "Failed to read from ~a into buffer: ~a" (file c) (error-string c))

(define-simple-condition decode-failed (mpg-file-string-error)
  ((in-buffer :initarg :in-buffer :reader in-buffer)
   (out-buffer :initarg :out-buffer :reader out-buffer)
   (in-size :initarg :in-size :reader in-size)
   (out-size :initarg :out-size :reader out-size))
  "Failed to decode requested data using ~a: ~a" (file c) (error-string c))

(define-simple-condition frame-decode-failed (mpg-file-string-error)
  "Frame decode requested on ~a failed: ~a" (file c) (error-string c))

(define-direct-condition query-failed (mpg-file-error generic-error)
  "Failed to query ~a from ~a~:[.~;: ~:*~a~]" (query c) (file c) (error-string c))

(define-direct-condition seek-failed (mpg-file-error)
  "Failed to seek ~a by ~a ~a to ~a." (file c) (by c) (mode c) (seek-position c))

(define-direct-condition equalizer-query-failed (mpg-file-error)
  "Failed to retrieve ~a equalizer band ~a on channel ~a." (file c) (band c) (channel c))

(define-direct-condition equalizer-set-failed (mpg-file-string-error)
  "Failed to set ~a equalizer band ~a on channel ~a to ~a: ~a" (file c) (band c) (channel c) (value c) (error-string c))

(define-simple-condition equalizer-reset-failed (mpg-file-string-error)
  "Failed to reset equalizer on ~a: ~a" (file c) (error-string c))

(define-simple-condition volume-query-failed (mpg-file-string-error)
  "Failed to query for volume on ~a: ~a" (file c) (error-string c))

(define-direct-condition volume-set-failed (mpg-file-string-error)
  "Failed to set volume on ~a ~:[absolutely~;relatively~] to ~a: ~a" (file c) (relative c) (value c) (error-string c))

(define-simple-condition scan-failed (mpg-file-string-error)
  "Failed to scan through ~a: ~a" (file c) (error-string c))

(define-simple-condition id3-query-failed (mpg-file-string-error)
  "Failed to retrieve ID3 information from ~a: ~a" (file c) (error-string c))
