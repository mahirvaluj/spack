(defpackage :leb128
  (:use :cl)
  (:export :encode-signed
           :decode-signed))

(in-package :leb128)

(defun encode-signed (i)
  "Encode a 64-bit integer integer into a leb128 unsigned-8 buffer"
  (declare ((signed-byte 64) i))
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

(defun decode-signed (buf &key (start 0))
  "decode integer from buffer"
  (let ((result 0) (shift 0) (curr) (cont t) (counter 0))
    (loop while cont do 
         (setf curr (aref buf start))
         (setf start (+ 1 start))
         (setf result (logior result (ash (logand curr #x7f) shift)))
         (setf shift (+ 7 shift))
         (incf counter)
         (when (= 0 (logand curr #x80))
           (if (= 64 (logand curr #x40))
               (return-from decode-signed (values (logior result (ash (lognot 0) shift)) counter))
               (return-from decode-signed (values result counter)))))))
