#|
 This file is a part of cl-mpg123
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

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

(defmacro with-error ((err datum &rest datum-args) &body form)
  `(let ((,err (progn ,@form)))
     (unless (eql ,err :ok)
       (let ((,err (if (eql ,err :err)
                       "Unknown error."
                       (cl-mpg123-cffi:plain-strerror ,err))))
         (error ,datum ,@datum-args)))))

(defmacro with-generic-error (&body form)
  (let ((err (gensym "ERR")))
    `(with-error (,err "~s failed: ~a" ',form ,err)
       ,@form)))

(defmacro with-negative-error (&body form)
  (let ((res (gensym "RES")))
    `(let ((,res (progn ,@form)))
       (when (< ,res 0) (error "Failed to execute ~s." ',form))
       ,res)))

(defmacro with-zero-error (&body form)
  (let ((res (gensym "RES")))
    `(let ((,res (progn ,@form)))
       (when (= ,res 0) (error "Failed to execute ~s." ',form))
       ,res)))
