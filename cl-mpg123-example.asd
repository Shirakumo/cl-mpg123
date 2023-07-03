(asdf:defsystem cl-mpg123-example
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "The ported mpg123-to-out123 example."
  :homepage "https://Shirakumo.github.io/cl-mpg123/"
  :bug-tracker "https://github.com/Shirakumo/cl-mpg123/issues"
  :source-control (:git "https://github.com/Shirakumo/cl-mpg123.git")
  :serial T
  :components ((:file "example"))
  :depends-on (:cl-mpg123
               :cl-out123
               :verbose))
