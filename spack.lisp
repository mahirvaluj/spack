(defpackage :spack
  (:use :cl)
  (:export :spack-elem :spack :elements :val :elem-type
           :spush :out :parse :make-and-push))

;; (ql:quickload '(:ieee-floats :trivial-utf-8 :cl-intbytes :ironclad :cl-leb128))

(in-package :spack)

(defclass spack-elem ()
  ((elem-type;; :integer, :float32, :float64, :byte, :string, (:array type), '(type1 type2 type3)
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
  (spush (make-instance 'spack-elem :elem-type :float32 :val elem) :spack-elem packet))

(defmethod spush ((elem double-float) (type (eql :float64)) (packet spack))
  "Push a float64 onto spack."
  (spush (make-instance 'spack-elem :elem-type :float64 :val elem) :spack-elem packet))

(defmethod spush ((elem integer) (type (eql :byte)) (packet spack))
  "Push a single byte onto spack"
  (when (or (< elem 0) (> elem 256))
    (error "byte element is larger than 256 or less than 0"))
  (spush (make-instance 'spack-elem :elem-type :byte :val elem) :spack-elem packet))

(defmethod spush ((elem string) (type (eql :string)) (packet spack))
  "Push a string encoded as utf-8 onto spack"
  (spush (make-instance 'spack-elem
                        :elem-type :string
                        :val elem)
         :spack-elem packet))

(defmethod spush ((elem array) (type (eql :string)) (packet spack))
  "Push a string already encoded as utf-8 onto spack"
  (loop for i across elem do (assert (and (> i 0) (< i 256))))
  (spush (make-instance 'spack-elem
                        :elem-type :string
                        :val (trivial-utf-8:utf-8-bytes-to-string elem))
         :spack-elem packet))

(defmethod spush ((elem array) (type (eql :byte-array)) (packet spack))
  "Special case to make a byte-array (I am pretty sure this would be
 useful)"
  (loop for i across elem do (assert (and (> i 0) (< i 256))))
  (spush (make-instance 'spack-elem
                        :elem-type '(:array :byte)
                        :val elem)
         :spack-elem packet))

(defmacro make-and-push (&rest elems)
  "Convenience macro to allow for creating throwaway spack objects
Pass this function tuples for val and type (make-and-push (val :type) (val :type) etc)"
  (let ((sp (gensym)))
    `(let ((,sp (make-instance 'spack:spack)))
       ,@(loop for i in elems collect `(spack:spush ,(car i) ,(cadr i) ,sp))
       ,sp)))

(defun parse-array (a)
  "Parses array into a spack-elem object. Returns values [value type]."
  (cond ((typep (aref a 0) 'integer)
         (values a '(:array :integer)))
        ((typep (aref a 0) 'single-float)
         (values a '(:array :float32)))
        ((typep (aref a 0) 'double-float)
         (values a '(:array :float64)))
        ((typep (aref a 0) 'string)
         (error "Arrays cannot contain strings!"))
        (t
         (error "Bad type passed to array!"))))

(defmethod spush ((elem array) (type (eql :array)) (packet spack))
  "Push an array onto spack, all elements must have the same type (can
   be struct)"
  (multiple-value-bind (a type) (parse-array elem)
    (spush (make-instance 'spack-elem
                          :elem-type type
                          :val a)
           :spack-elem packet)))

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
              (vector-push-buf-extend (cl-intbytes:int32->octets (ieee-floats:encode-float32 (val elem)))
                                      elembuf)))
           ((eq (elem-type elem) :float64)
            (progn
              (vector-push-extend #x03 typebuf)
              (vector-push-buf-extend (cl-intbytes:int64->octets (ieee-floats:encode-float64 (val elem)))
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
              (vector-push-buf-extend (trivial-utf-8:string-to-utf-8-bytes (val elem))
                                      elembuf)))
           ((and (consp (elem-type elem)) (eq (car (elem-type elem)) :array))
            (let ((atype (case (cadr (elem-type elem))
                           (:integer #x01) (:float32 #x02)
                           (:float64 #x03) (:byte #x04) (t (error "Bad type in array")))))
              (vector-push-extend #x10 typebuf)
              (vector-push-extend atype typebuf)
              (vector-push-buf-extend (leb128:encode-signed (length (val elem))) typebuf)
              (cond ((= atype #x1)
                     (loop for ai across (val elem) do (vector-push-buf-extend (leb128:encode-signed ai) elembuf)))
                    ((= atype #x2)
                     (loop for ai across (val elem) do (vector-push-buf-extend (cl-intbytes:int32->octets (ieee-floats:encode-float32 ai)) elembuf)))
                    ((= atype #x3)
                     (loop for ai across (val elem) do (vector-push-buf-extend (cl-intbytes:int64->octets (ieee-floats:encode-float64 ai)) elembuf)))
                    ((= atype #x4)
                     (loop for ai across (val elem) do (vector-push-extend ai elembuf))))))
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

(defmethod parse ((in stream))
  (let ((buf (make-array 512 :fill-pointer 0 :element-type '(unsigned-byte 8) :adjustable t))
        (typelen) (varlen))
    (loop for i from 0 below 32 do (vector-push-extend (read-byte in) buf))
    (setf typelen (leb128:decode-signed in)
          varlen (leb128:decode-signed in))
    (loop for i from 0 below typelen do (vector-push-extend (read-byte in) buf))
    (loop for i from 0 below varlen do (vector-push-extend (read-byte in) buf))
    (parse buf)))

(defmethod parse ((buf array))
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
              (progn
                (incf i)
                (let ((atype) (alen) (a))
                  (parse-and-increment atype i
                    (values (aref buf i) 1))
                  (parse-and-increment alen i
                    (leb128:decode-signed buf :start i))
                  (setf a (make-array alen))
                  (cond ((eql atype #x01)
                         (let ((x))
                           (loop for ai from 0 below alen do
                                (parse-and-increment x vi (leb128:decode-signed buf :start vi))
                                (setf (aref a ai) x))
                           (spush a :array spack)))
                        ((eql atype #x02)
                         (let ((x))
                           (loop for ai from 0 below alen do
                                (parse-and-increment x vi
                                  (values (ieee-floats:decode-float32 (cl-intbytes:octets->int32 (subseq buf vi (+ vi 4))))
                                          4))
                                (setf (aref a ai) x))
                           (spush a :array spack)))
                        ((eql atype #x03)
                         (let ((x))
                           (loop for ai from 0 below alen do
                                (parse-and-increment x vi
                                  (values (ieee-floats:decode-float64 (cl-intbytes:octets->int64 (subseq buf vi (+ vi 8))))
                                          8))
                                (setf (aref a ai) x))
                           (spush a :array spack)))
                        ((eql atype #x04)
                         (let ((x))
                           (loop for ai from 0 below alen do
                                (parse-and-increment x vi
                                  (values (aref buf vi) 1))
                                (setf (aref a ai) x))
                           (spush a :byte-array spack)))
                        (t (error (format nil "bad type ~A found when parsing array" atype)))))))
             (t
              (error (format nil "bad type ~A found in buffer being parsed at index ~A" (aref buf i) i))))))
    spack))

