(defpackage :spack
  (:use :cl)
  (:export :spack-elem :spack
           :spush :out))

;; (ql:quickload '(:ieee-floats :trivial-utf-8 :cl-intbytes :ironclad))

(in-package :spack)

(defclass spack-elem ()
  ((elem-type;; :integer, :float32, :float64, :byte, :string, :array '(type num), '(type1 type2 type3)
     :initarg :elem-type
     :accessor elem-type)
   (val
    :initarg :val
    :accessor val)))

(defclass spack ()
  ((elements
    :type array
    :initform (make-array 10 :fill-pointer 0 :adjustable t)
    :accessor elements)))

(defmethod spush ((elem spack-elem) (type (eql :spack-elem)) (packet spack))
  "Push spack-elem onto a spack"
  (vector-push-extend elem (elements packet)))

(defmethod spush ((elem integer) (type (eql :integer)) (packet spack))
  "Push an integer onto spack"
  (spush (make-instance 'spack-elem :elem-type :integer :val elem) :spack-elem packet))

(defmethod spush ((elem single-float) (type (eql :float32)) (packet spack))
  "Push a float32 onto spack."
  (spush (make-instance 'spack-elem :elem-type :float32 :val (ieee-floats:encode-float32 elem)) :spack-elem packet))

(defmethod spush ((elem double-float) (type (eql :float64)) (packet spack))
  "Push a float64 onto spack."
  (spush (make-instance 'spack-elem :elem-type :float64 :val (ieee-floats:encode-float64 elem)) :spack-elem packet))

(defmethod spush ((elem integer) (type (eql :byte)) (packet spack))
  "Push a single byte onto spack"
  (when (or (< elem 0) (> elem 256))
    (error "byte element is larger than 256 or less than 0"))
  (spush (make-instance 'spack-elem :elem-type :byte :val elem) :spack-elem packet))

(defmethod spush ((elem string) (type (eql :string)) (packet spack))
  "Push a string encoded as utf-8 onto spack"
  (spush (make-instance 'spack-elem
                        :elem-type :string
                        :val (trivial-utf-8:string-to-utf-8-bytes elem))
         :spack-elem packet))

(defmethod spush ((elem array) (type (eql :array)) (packet spack))
  "Push an array onto spack, all elements must have the same type (can
   be struct)"
  (error "not implemented yet"))

(defmethod spush ((elem list) (type (eql :group)) (packet spack))
  "Push a group of items onto spack (represented as a list of items)"
  (error "not implemented yet"))

(defun vector-push-buf-extend (buf vec)
  "This loops over buf and pushes each item onto vec"
  (loop for i across buf do
       (vector-push-extend i vec)))

(defmethod out ((packet spack))
  "format a packet out of each elem on spack"
  (let ((typebuf (make-array 256 :fill-pointer 0 :element-type '(unsigned-byte 8) :adjustable t))
        (elembuf (make-array 256 :fill-pointer 0 :element-type '(unsigned-byte 8) :adjustable t)))
    (loop for elem across (elements packet)
       do
         (cond
           ((eq (elem-type elem) :integer)
            (progn
              (vector-push-extend #x01 typebuf)
              (vector-push-buf-extend (leb128:encode-signed (val elem))
                                      elembuf)))
           ((eq (elem-type elem) :float32)
            (progn
              (vector-push-extend #x02 typebuf)
              (vector-push-buf-extend (cl-intbytes:int32->octets (val elem))
                                      elembuf)))
           ((eq (elem-type elem) :float64)
            (progn
              (vector-push-extend #x02 typebuf)
              (vector-push-buf-extend (cl-intbytes:int64->octets (val elem))
                                      elembuf)))
           ((eq (elem-type elem) :byte)
            (progn
              (vector-push-extend #x03 typebuf)
              (vector-push-extend (val elem) elembuf)))
           ((eq (elem-type elem) :string)
            (progn
              (vector-push-extend #x04 typebuf)
              (vector-push-buf-extend (leb128:encode-signed (length (val elem)))
                                      typebuf)
              (vector-push-buf-extend (val elem)
                                      elembuf)))
           ((eq (elem-type elem) :array)
            (error "not implemented yet")
            )
           ((eq (elem-type elem) :group)
            (error "not implemented yet"))))
    ;; NOTE: This is badly done, but ironclad doesn't support non-simple vectors??
    (let ((buf (concatenate '(vector (unsigned-byte 8))
                            (leb128:encode-signed (length typebuf))
                            (leb128:encode-signed (length elembuf))
                            typebuf
                            elembuf)))
      (concatenate '(vector (unsigned-byte 8))
                   (ironclad:digest-sequence :sha256 buf)
                   buf))))

