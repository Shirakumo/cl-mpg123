(in-package #:org.shirakumo.fraf.mpg123)

(defmacro with-foreign-values (bindings &body body)
  `(with-foreign-objects ,bindings
     ,@body
     (values ,@(loop for (name type) in bindings
                     collect `(unless (null-pointer-p ,name) (mem-ref ,name ,type))))))

(defmacro with-value-args (bindings call &body body)
  `(multiple-value-bind ,(mapcar #'first bindings)
       (with-foreign-values ,bindings ,call)
     ,@body))

(defmacro with-error ((err datum &rest datum-args &key (ok ''(:ok)) &allow-other-keys) &body form)
  (let ((args (copy-list datum-args)))
    (remf args :ok)
    `(let ((,err (progn ,@form)))
       (unless (find ,err ,ok)
         (cerror "Ignore the error." ,datum ,@args)))))

(defmacro with-generic-error (&body form)
  (let ((err (gensym "ERR")))
    `(with-error (,err 'generic-error :form ',form :error ,err)
       ,@form)))

(defmacro with-negative-error ((datum &rest datum-args) &body form)
  (let ((res (gensym "RES")))
    `(let ((,res (progn ,@form)))
       (when (< ,res 0) (error ,datum ,@datum-args))
       ,res)))

(defmacro with-zero-error ((datum &rest datum-args) &body form)
  (let ((res (gensym "RES")))
    `(let ((,res (progn ,@form)))
       (when (= ,res 0) (error ,datum ,@datum-args))
       ,res)))

(defun string-nil (string)
  (when (and string (string/= "" string))
    string))

(defun direct-str (pointer length &optional (encoding :utf-8))
  (string-nil (or (ignore-errors (foreign-string-to-lisp pointer :max-chars length :encoding encoding))
                  (ignore-errors (foreign-string-to-lisp pointer :max-chars length :encoding :iso-8859-1)))))

(defun mstring (mstring)
  (string-nil
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
       :encoding :UTF-8)))))

(defun split (string char)
  (let ((parts ()))
    (loop with buf = (make-string-output-stream)
          for c across string
          do (if (char= c char)
                 (let ((part (get-output-stream-string buf)))
                   (when (string-nil part) (push part parts)))
                 (write-char c buf))
          finally (push (get-output-stream-string buf) parts))
    (nreverse parts)))
