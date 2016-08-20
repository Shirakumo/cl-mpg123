#|
 This file is a part of cl-mpg123
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mpg123)

(defvar *init* NIL)

(defun init ()
  (unless *init*
    (with-generic-error (cl-mpg123-cffi:init))
    (setf *init* T)))

(defun exit ()
  (when *init*
    (cl-mpg123-cffi:exit)
    (setf *init* NIL)))

(defun encode-encodings (encodings)
  (etypecase encodings
    (integer encodings)
    (list (let ((encoding 0))
            (dolist (enc encodings encoding)
              (setf encoding (logior encoding (foreign-enum-value 'cl-mpg123-cffi:enc enc))))))))

(defun encode-channels (channels)
  (etypecase channels
    (integer channels)
    (list (let ((channel 0))
            (dolist (chan channels channel)
              (setf channel (logior channel (foreign-enum-value 'cl-mpg123-cffi:channelcount chan))))))))

(defun decode-flags (flags)
  (loop for flag in (foreign-enum-keyword-list 'cl-mpg123-cffi:flags)
        when (/= 0 (logand flags (foreign-enum-value 'cl-mpg123-cffi:flags flag)))
        collect flag))

(defun dispose-handle (handle)
  (cl-mpg123-cffi:close handle)
  (cl-mpg123-cffi:delete handle))

(defclass file ()
  ((handle :initform NIL :reader handle)
   (connected :initform NIL :reader connected :writer set-connected)
   (scanned :initform NIL :reader scanned :writer set-scanned)
   (buffer :initform NIL :reader buffer)
   (rate :initform NIL :reader rate)
   (channels :initform NIL :reader channels)
   (encoding :initform NIL :reader encoding)
   (path :initarg :path :reader path)
   (decoder :initarg :decoder :reader decoder)
   (accepted-format :initarg :accepted-format :reader accepted-format)
   (buffer-size :initarg :buffer-size :reader buffer-size))
  (:default-initargs
   :path NIL
   :decoder NIL
   :accepted-format T
   :buffer-size T))

(defmethod print-object ((file file) stream)
  (print-unreadable-object (file stream :type T)
    (format stream "~@[~s~]~:[~; :CONNECTED~]"
            (path file) (connected file))))

(defmethod shared-initialize :after ((file file) slots &key decoder accepted-format buffer-size)
  (init)
  (with-foreign-object (err :pointer)
    (let ((handle (cl-mpg123-cffi:new (or decoder (null-pointer)) err)))
      (when (null-pointer-p handle)
        (error "Failed to create file handle: ~a" (cl-mpg123-cffi:plain-strerror err)))
      (setf (slot-value file 'handle) handle)
      (tg:finalize file (lambda () (dispose-handle handle)))
      (etypecase accepted-format
        ((eql NIL) (cl-mpg123-cffi:format-none handle))
        ((eql T)   (cl-mpg123-cffi:format-all handle))
        (list (destructuring-bind (rate channels encodings) accepted-format
                (cl-mpg123-cffi:format handle rate (encode-channels channels) (encode-encodings encodings)))))
      (etypecase buffer-size
        ((eql T) (setf (slot-value file 'buffer-size) (cl-mpg123-cffi:outblock handle)))
        ((eql NIL))
        (integer))
      (when buffer-size
        (let ((buffer (foreign-alloc :char :count (buffer-size file))))
          (setf (slot-value file 'buffer) buffer)
          (tg:finalize file (lambda () (foreign-free buffer)))))))
  file)

(defmethod reinitialize-instance :around ((file file) &key)
  (dispose-handle (handle file))
  (call-next-method)
  (set-scanned NIL file)
  (when (connected file)
    (set-connected NIL file)
    (connect file))
  file)

(defun check-connected (file)
  (unless (connected file)
    (error "~a is not connected yet!" file)))

(defun connect (file &key (path (path file)))
  (with-generic-error
      (cl-mpg123-cffi:open
       (handle file)
       (etypecase path
         (string path)
         (pathname (uiop:native-namestring path)))))
  (set-connected T file)
  (setf (slot-value file 'path) path)
  (multiple-value-bind (rate channels encoding) (file-format file)
    (setf (slot-value file 'rate) rate)
    (setf (slot-value file 'channels) channels)
    (setf (slot-value file 'encoding) encoding))
  file)

(defun disconnect (file)
  (check-connected file)
  (with-generic-error
      (cl-mpg123-cffi:close (handle file)))
  (set-connected NIL file)
  file)

(defun (setf decoder) (decoder file)
  (with-generic-error (cl-mpg123-cffi:decoder (handle file) decoder))
  decoder)

(defun decoders ()
  (loop for ptr = (cl-mpg123-cffi:decoders) then (inc-pointer ptr (foreign-type-size :pointer))
        for string = (mem-ref ptr :string)
        while string collect string))

(defun supported-decoders ()
  (loop for ptr = (cl-mpg123-cffi:supported-decoders) then (inc-pointer ptr (foreign-type-size :pointer))
        for string = (mem-ref ptr :string)
        while string collect string))

(defun supported-rates ()
  (with-value-args ((list :pointer) (number 'size_t))
      (cl-mpg123-cffi:rates list number)
    (when (and list number)
      (loop for i from 0 below number
            collect (mem-aref list :long i)))))

(defun supported-encodings ()
  (with-value-args ((list :pointer) (number 'size_t))
      (cl-mpg123-cffi:encodings list number)
    (when (and list number)
      (loop for i from 0 below number
            for enc = (mem-aref list :int i)
            collect (list (foreign-enum-keyword 'cl-mpg123-cffi:enc enc)
                          (cl-mpg123-cffi:encsize enc))))))

(defun file-format (file)
  (with-foreign-values ((rate :long) (channels :int) (encoding 'cl-mpg123-cffi:enc))
    (with-generic-error (cl-mpg123-cffi:getformat (handle file) rate channels encoding))))

(defun read-directly (file buffer-pointer buffer-size)
  (with-foreign-object (done 'size_t)
    (with-generic-error (cl-mpg123-cffi:read (handle file) buffer-pointer buffer-size done))
    (mem-ref done 'size_t)))

(defun process (file &key (buffer (buffer file))
                          (buffer-size (buffer-size file)))
  (values (read-directly file buffer buffer-size)
          buffer))

(defun decode (file in in-size out out-size)
  (with-foreign-object (done 'size_t)
    (with-generic-error (cl-mpg123-cffi:decode (handle file) in in-size out out-size done))
    (mem-ref done 'size_t)))

(defun decode-frame (file)
  (with-foreign-values ((num 'off_t) (audio :pointer) (bytes 'size_t))
    (with-generic-error (cl-mpg123-cffi:decode-frame (handle file) num audio bytes))))

(defun sample-position (file)
  (with-negative-error (cl-mpg123-cffi:tell (handle file))))

(defun frame-position (file)
  (with-negative-error (cl-mpg123-cffi:tellframe (handle file))))

(defun stream-position (file)
  (with-negative-error (cl-mpg123-cffi:tell-stream (handle file))))

(defun seek (file position &key (mode :absolute) (by :sample))
  (let ((whence (ecase mode (:absolute :set) (:relative :cur) (:from-end :end))))
    (with-negative-error
      (ecase by
        (:sample (cl-mpg123-cffi:seek (handle file) position whence))
        (:frame (cl-mpg123-cffi:seek-frame (handle file) position whence))
        (:second (seek file (time-frame-index file position) :by :frame))))))

(defun time-frame-index (file seconds)
  (with-negative-error (cl-mpg123-cffi:timeframe (handle file) (float seconds 0.0d0))))

(defun equalizer (file channel band)
  (assert (<= 0 band 31) () "Equalizer band must be within [0,31].")
  (with-zero-error (cl-mpg123-cffi:geteq (handle file) channel band)))

(defun (setf equalizer) (value file channel band)
  (assert (<= 0 band 31) () "Equalizer band must be within [0,31].")
  (with-generic-error (cl-mpg123-cffi:eq (handle file) channel band value)))

(defun reset-equalizer (file)
  (with-generic-error (cl-mpg123-cffi:reset-eq (handle file))))

(defun volume (file)
  (with-foreign-values ((base :double) (really :double) (rva-db :double))
    (with-generic-error (cl-mpg123-cffi:getvolume (handle file) base really rva-db))))

(defun (setf volume) (volume file &key relative)
  (if relative
      (with-generic-error (cl-mpg123-cffi:volume-change (handle file) volume))
      (with-generic-error (cl-mpg123-cffi:volume (handle file) volume))))

(defun info (file)
  (with-foreign-object (info :pointer)
    (with-generic-error (cl-mpg123-cffi:info (handle file) info))
    (list :version (cl-mpg123-cffi:frameinfo-version info)
          :layer (cl-mpg123-cffi:frameinfo-layer info)
          :rate (cl-mpg123-cffi:frameinfo-rate info)
          :mode (cl-mpg123-cffi:frameinfo-mode info)
          :mode-ext (cl-mpg123-cffi:frameinfo-mode-ext info)
          :flags (decode-flags (cl-mpg123-cffi:frameinfo-flags info))
          :emphasis (cl-mpg123-cffi:frameinfo-emphasis info)
          :bitrate (cl-mpg123-cffi:frameinfo-bitrate info)
          :abr-rate (cl-mpg123-cffi:frameinfo-abr-rate info)
          :vbr (cl-mpg123-cffi:frameinfo-vbr info))))

(defun scan (file)
  (unless (scanned file)
    (with-generic-error (cl-mpg123-cffi:scan (handle file)))
    (set-scanned T file)
    file))

(defun frame-count (file)
  (scan file)
  (with-negative-error (cl-mpg123-cffi:framelength (handle file))))

(defun sample-count (file)
  (scan file)
  (with-negative-error (cl-mpg123-cffi:length (handle file))))

(defun frame-seconds (file)
  (scan file)
  (with-negative-error (cl-mpg123-cffi:tpf (handle file))))

(defun frame-samples (file)
  (scan file)
  (with-negative-error (cl-mpg123-cffi:spf (handle file))))

(defun track-length (file)
  (* (frame-seconds file)
     (frame-count file)))

(defun metadata (file)
  (scan file)
  (multiple-value-bind (id3v1 id3v2)
      (with-foreign-values ((id3v1 :pointer) (id3v2 :pointer))
        (with-generic-error (cl-mpg123-cffi:id3 (handle file) id3v1 id3v2)))
    (make-instance 'metadata :id3v1 (if (null-pointer-p id3v1) NIL id3v1)
                             :id3v2 (if (null-pointer-p id3v2) NIL id3v2))))
