(defsystem "spack"
  :depends-on (#:ieee-floats #:trivial-utf-8 #:cl-intbytes #:ironclad #:cl-leb128)
  :components ((:file "spack")))
