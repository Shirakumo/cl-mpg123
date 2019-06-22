#|
 This file is a part of cl-mpg123
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem cl-mpg123-example
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "The ported mpg123-to-out123 example."
  :homepage "https://Shirakumo.github.io/cl-mpg123/"
  :bug-tracker "https://github.com/Shirakumo/cl-mpg123/issues"
  :source-control (:git "https://github.com/Shirakumo/cl-mpg123.git")
  :serial T
  :components ((:file "example"))
  :depends-on (:cl-mpg123
               :cl-out123
               :verbose))
