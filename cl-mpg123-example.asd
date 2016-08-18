#|
 This file is a part of cl-mpg123
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem cl-mpg123-example
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "The ported mpg123-to-out123 example."
  :homepage "https://github.com/Shirakumo/cl-mpg123"
  :serial T
  :components ((:file "example"))
  :depends-on (:cl-mpg123
               :cl-out123
               :verbose))
