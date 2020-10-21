(defsystem "spack"
  :depends-on (#:ieee-floats #:trivial-utf-8 #:cl-intbytes #:ironclad)
  :components ((:file "leb128")
               (:file "spack")))
