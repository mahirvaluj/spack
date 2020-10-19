(defpackage :leb128
  (:use :cl)
  (:export :encode-signed
           :decode-signed))

(in-package :leb128)

(defun encode-signed (i)
  "Encode an integer into a leb128 unsigned-8 buffer"
  (let ((more t) (curr) (ret (make-array 16 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))) ;(neg (< i 0))
    (loop while more do
         ;; (format t "i: ~A ~%" i)
         (setf curr (logand i #x7f))
         (setf i (ash i -7))
         ;; (format t "curr: ~A i: ~A ~%" curr i)
         (if (or (and (= i 0)  (= (logand curr #x40) 0))
                 (and (= i -1) (= (logand curr #x40) 64)))
             (setf more nil)
             (setf curr (logior curr #x80)))
         (vector-push-extend curr ret))
    ret))

(defun decode-signed (buf)
  "decode integer from buffer"
  (error "not implemented yet"))
