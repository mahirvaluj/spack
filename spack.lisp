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

(defmethod spush ((elem array) (type (eql :string)) (packet spack))
  "Push a string already encoded as utf-8 onto spack"
  (loop for i across elem do (assert (and (> i 0) (< i 256))))
  (spush (make-instance 'spack-elem
                        :elem-type :string
                        :val elem)
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
              (vector-push-extend #x03 typebuf)
              (vector-push-buf-extend (cl-intbytes:int64->octets (val elem))
                                      elembuf)))
           ((eq (elem-type elem) :byte)
            (progn
              (vector-push-extend #x04 typebuf)
              (vector-push-extend (val elem) elembuf)))
           ((eq (elem-type elem) :string)
            (progn
              (vector-push-extend #x05 typebuf)
              (vector-push-buf-extend (leb128:encode-signed (length (val elem)))
                                      typebuf)
              (vector-push-buf-extend (val elem)
                                      elembuf)))
           ((eq (elem-type elem) :array)
            (error "not implemented yet")
            )
           ((eq (elem-type elem) :group)
            (error "not implemented yet"))
           (t (error (format nil "bad type ~A passed to out" (elem-type elem))))))
    ;; NOTE: This is badly done, but ironclad doesn't support non-simple vectors??
    (let ((buf (concatenate '(vector (unsigned-byte 8))
                            (leb128:encode-signed (length typebuf))
                            (leb128:encode-signed (length elembuf))
                            typebuf
                            elembuf)))
      (concatenate '(vector (unsigned-byte 8))
                   (ironclad:digest-sequence :sha256 buf)
                   buf))))

(defun verify-sha-integrity (buf)
  "Verifies sha-256 at start of spack packet"
  (let ((digest (ironclad:digest-sequence :sha256 buf :start 32)))
    (loop for i from 0 below (length digest)
       do (unless (= (aref buf i) (aref digest i)) (return-from verify-sha-integrity nil)))
    t))

(defmacro with-gensyms (syms &body body)
  `(let ,(loop for s in syms collect `(,s (gensym)))
    ,@body))

(defmacro parse-and-increment (capture-var index &body parse-body)
  "Parse and increment expects a function to return at least two
things. A value, and an integer"
  (with-gensyms (parsed val)
    `(multiple-value-bind (,val ,parsed) (progn ,@parse-body)
       (setf ,index (+ ,index ,parsed))
       (setf ,capture-var ,val))))

;; this macro isn't actually necessary but  thought it was pretty cool
(defmacro multiple-value-setf ((&rest vars) form)
  (let ((tag-acc))
    (loop for i in vars do (push (gensym) tag-acc))
    `(multiple-value-bind (,@tag-acc) ,form
       ,@(map 'list #'(lambda (var val) `(setf ,var ,val)) vars tag-acc))))


(defun parse (buf)
  (unless (verify-sha-integrity buf)
    (error "data corruption has occured. Hash digest does not check
    out."))
  (let ((spack (make-instance 'spack)) (i 32) (types-size) (vals-size) (vi) (types-start))
    (parse-and-increment types-size i (leb128:decode-signed buf :start i))
    (parse-and-increment vals-size i (leb128:decode-signed buf :start i))
    (setf types-start i)
    (setf vi (+ types-start types-size))
    (loop while (< i (+ types-size types-start)) do
         (let ((val))
           (cond
             ((eq (aref buf i) #x01)
              (progn
                (incf i)
                (parse-and-increment val vi
                  (leb128:decode-signed buf :start vi))
                (spush val :integer spack)))
             ((eq (aref buf i) #x02)
              (progn
                (incf i)
                (parse-and-increment val vi
                  (values (ieee-floats:decode-float32 (cl-intbytes:octets->int32 (subseq buf vi (+ vi 4))))
                          4))
                (spush val :float32 spack)))
             ((eq (aref buf i) #x03)
              (progn
                (incf i)
                (parse-and-increment val vi
                  (values (ieee-floats:decode-float64 (cl-intbytes:octets->int64 (subseq buf vi (+ vi 8))))
                          8))
                (spush val :float64 spack)))
             ((eq (aref buf i) #x04)
              (progn
                (incf i)
                (spush (aref buf vi) :byte spack)
                (incf vi)))
             ((eq (aref buf i) #x05)
              (progn
                (incf i)
                (let ((slen) (s))
                  (parse-and-increment slen i
                    (leb128:decode-signed buf :start i))
                  (parse-and-increment s vi
                    (values (subseq buf vi (+ vi slen))
                            slen))
                  (spush s :string spack))))
             ((eq (aref buf i) #x10)
              (error "Not implemented yet!"))
             ((eq (aref buf i) #x20)
              (error "Not implemented yet!"))
             (t
              (error (format nil "bad type ~A found in buffer being parsed at index ~A" (aref buf i) i))))))
    spack))
