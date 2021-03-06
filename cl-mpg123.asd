#|
 This file is a part of cl-mpg123
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem cl-mpg123
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Bindings to libmpg123, providing cross-platform, fast MPG1/2/3 decoding."
  :homepage "https://Shirakumo.github.io/cl-mpg123/"
  :bug-tracker "https://github.com/Shirakumo/cl-mpg123/issues"
  :source-control (:git "https://github.com/Shirakumo/cl-mpg123.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "conditions")
               (:file "low-level")
               (:file "id3-data")
               (:file "metadata")
               (:file "wrapper")
               (:file "documentation"))
  :depends-on (:cffi
               :trivial-features
               :trivial-garbage
               :documentation-utils))
