;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
#|

 Floating Point Functions

 Copyright (c) 2009-2012, Thomas M. Hermann
 All rights reserved.

 Redistribution and  use  in  source  and  binary  forms, with or without
 modification, are permitted  provided  that the following conditions are
 met:

   o  Redistributions of  source  code  must  retain  the above copyright
      notice, this list of conditions and the following disclaimer.
   o  Redistributions in binary  form  must reproduce the above copyright
      notice, this list of  conditions  and  the  following disclaimer in
      the  documentation  and/or   other   materials  provided  with  the
      distribution.
   o  The names of the contributors may not be used to endorse or promote
      products derived from this software without  specific prior written
      permission.

 THIS SOFTWARE IS  PROVIDED  BY  THE  COPYRIGHT  HOLDERS AND CONTRIBUTORS
 "AS IS"  AND  ANY  EXPRESS  OR  IMPLIED  WARRANTIES, INCLUDING,  BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES  OF MERCHANTABILITY AND FITNESS FOR A
 PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 EXEMPLARY, OR  CONSEQUENTIAL  DAMAGES  (INCLUDING,  BUT  NOT LIMITED TO,
 PROCUREMENT OF  SUBSTITUTE  GOODS  OR  SERVICES;  LOSS  OF USE, DATA, OR
 PROFITS; OR BUSINESS INTERRUPTION)  HOWEVER  CAUSED AND ON ANY THEORY OF
 LIABILITY, WHETHER  IN  CONTRACT,  STRICT  LIABILITY, OR TORT (INCLUDING
 NEGLIGENCE OR  OTHERWISE)  ARISING  IN  ANY  WAY  OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 References
 [NumAlgoC] Gisela Engeln-Mullges and Frank Uhlig "Numerical
            Algorithms with C", Springer, 1996
            ISBN: 3-540-60530-4

|#

(in-package :floating-point)

(defvar *epsilon* nil
  "The default error epsilon.")

(defvar *significant-figures* 4
  "The default number of significant figures.")

(defgeneric float-equal (data1 data2 &optional epsilon)
  (:documentation
   "Return true if the floating point data is equal."))

(defgeneric norm-equal (data1 data2 &optional epsilon measure)
  (:documentation
   "Return true if the norm of the data is equal."))

(defgeneric sigfig-equal (data1 data2 &optional significant-figures)
  (:documentation
   "Return true if the data have equal significant figures."))

;;; (FLOAT-EQUAL data1 data2 epsilon) => true or false
(defun %float-equal (data1 data2 epsilon)
  "Return true if the relative error between the data is less than
epsilon."
  (or
   (and (zerop data1) (zerop data2))
   (< (%relative-error data1 data2) epsilon)))

(defmethod float-equal ((data1 float) (data2 float)
                        &optional (epsilon *epsilon*))
  "Return true if the relative error between data1 and data2 is less
than epsilon."
  (%float-equal data1 data2
                (or epsilon (max (default-epsilon data1)
                                 (default-epsilon data2)))))

(defmethod float-equal ((data1 float) (data2 rational)
                        &optional (epsilon *epsilon*))
  "Return true if the relative error between data1 and data2 is less
than epsilon."
  (%float-equal data1 (float data2 data1)
                (or epsilon (default-epsilon data1))))

(defmethod float-equal ((data1 rational) (data2 float)
                        &optional (epsilon *epsilon*))
  "Return true if the relative error between data1 and data2 is less
than epsilon."
  (%float-equal (float data1 data2) data2
                (or epsilon (default-epsilon data2))))

(defmethod float-equal ((data1 float) (data2 complex)
                        &optional (epsilon *epsilon*))
  "Return true if the relative error between data1 and data2 is less
than epsilon."
  (%float-equal data1 data2
                (or epsilon (max (default-epsilon data1)
                                 (default-epsilon data2)))))

(defmethod float-equal ((data1 complex) (data2 float)
                        &optional (epsilon *epsilon*))
  "Return true if the relative error between data1 and data2 is less
than epsilon."
  (%float-equal data1 data2
                (or epsilon (max (default-epsilon data1)
                                 (default-epsilon data2)))))

(defmethod float-equal ((data1 complex) (data2 complex)
                        &optional (epsilon *epsilon*))
  "Return true if the relative error between data1 and data2 is less
than epsilon."
  (< (relative-error data1 data2)
     (or epsilon (max (default-epsilon data1)
                      (default-epsilon data2)))))

(defun %seq-float-equal (seq1 seq2 epsilon)
  "Return true if the element-wise comparison of relative error is
less than epsilon."
  (or
   (and (null seq1) (null seq2))
   (when (= (length seq1) (length seq2))
     (every
      (lambda (d1 d2) (float-equal d1 d2 epsilon)) seq1 seq2))))

(defmethod float-equal ((data1 list) (data2 list)
                        &optional (epsilon *epsilon*))
  "Return true if the lists are equal in length and element-wise
comparison of the relative error is less than epsilon."
  (%seq-float-equal data1 data2 epsilon))

(defmethod float-equal ((data1 list) (data2 vector)
                        &optional (epsilon *epsilon*))
  "Return true if the vector and the list are equal in length and
element-wise comparison of the relative error is less than epsilon."
  (%seq-float-equal data1 data2 epsilon))

(defmethod float-equal ((data1 vector) (data2 list)
                        &optional (epsilon *epsilon*))
  "Return true if the vector and the list are equal in length and
element-wise comparison of the relative error is less than epsilon."
  (%seq-float-equal data1 data2 epsilon))

(defmethod float-equal ((data1 vector) (data2 vector)
                        &optional (epsilon *epsilon*))
  "Return true if the vectors are equal in length and element-wise
comparison of the relative error is less than epsilon."
  (%seq-float-equal data1 data2 epsilon))

(defmethod float-equal ((data1 array) (data2 array)
                        &optional (epsilon *epsilon*))
  "Return true if the arrays are equal in length and element-wise
comparison of the relative error is less than epsilon."
  (when (equal (array-dimensions data1)
               (array-dimensions data2))
    (%seq-float-equal
     (make-array (array-total-size data1)
                 :element-type (array-element-type data1)
                 :displaced-to data1)
     (make-array (array-total-size data2)
                 :element-type (array-element-type data2)
                 :displaced-to data2)
     epsilon)))

;;; (NORM-EQUAL data1 data2 epsilon measure) => boolean
(defun %norm-equal (seq1 seq2 epsilon measure)
  "Return true if the relative error norm is less than epsilon."
  (or
   (and (null seq1) (null seq2))
   (< (%relative-error-norm seq1 seq2 measure) epsilon)))

(defmethod norm-equal ((data1 list) (data2 list) &optional
                       (epsilon *epsilon*) (measure *measure*))
  "Return true if the lists are equal in length and the relative error
norm is less than epsilon."
  (%norm-equal data1 data2 epsilon measure))

(defmethod norm-equal ((data1 list) (data2 vector) &optional
                       (epsilon *epsilon*) (measure *measure*))
  "Return true if the vector and the list are equal in length and the
relative error norm is less than epsilon."
  (%norm-equal data1 data2 epsilon measure))

(defmethod norm-equal ((data1 vector) (data2 list) &optional
                       (epsilon *epsilon*) (measure *measure*))
  "Return true if the vector and the list are equal in length and the
relative error norm is less than epsilon."
  (%norm-equal data1 data2 epsilon measure))

(defmethod norm-equal ((data1 vector) (data2 vector) &optional
                       (epsilon *epsilon*) (measure *measure*))
  "Return true if the vectors are equal in length and the relative
error norm is less than epsilon."
  (%norm-equal data1 data2 epsilon measure))

(defmethod norm-equal ((data1 array) (data2 array) &optional
                       (epsilon *epsilon*) (measure *measure*))
  "Return true if the arrays are equal in length and the relative
error norm is less than epsilon."
  (when (equal (array-dimensions data1)
               (array-dimensions data2))
    (%norm-equal
     (make-array (array-total-size data1)
                 :element-type (array-element-type data1)
                 :displaced-to data1)
     (make-array (array-total-size data2)
                 :element-type (array-element-type data2)
                 :displaced-to data2)
     epsilon measure)))

;;; (NORMALIZE-FLOAT significand &optional exponent) => significand,exponent
;;; [NumAlgoC] : Definition 1.7, pg. 4
;;;
;;; To avoid using 0.1, first 1.0 <= significand < 10. On the final
;;; return, scale 0.1 <= significand < 1.
(defun %normalize-float (significand &optional (exponent 0))
  "Return the normalized floating point number and exponent."
  (cond
    ((zerop significand)
     (values significand 0))
    ((>= (abs significand) 10)
     (%normalize-float (/ significand 10.0) (1+ exponent)))
    ((< (abs significand) 1)
     (%normalize-float (* significand 10.0) (1- exponent)))
    (t (values (/ significand 10.0) (1+ exponent)))))

;;; (SIGFIG-EQUAL float1 float2 significant-figures) => true or false
(defun %sigfig-equal (float1 float2 significant-figures)
  "Return true if the floating point numbers have equal significant
figures."
  (if (or (zerop float1) (zerop float2))
      (< (abs (+ float1 float2)) (* 5D-1 (expt 1D1 (- significant-figures))))
      (multiple-value-bind (sig1 exp1) (%normalize-float float1)
        (multiple-value-bind (sig2 exp2) (%normalize-float float2)
          (= (round (* sig1 (expt 1D1 significant-figures)))
             (round (* sig2 (expt 1D1 (- significant-figures (- exp1 exp2))))))))))

(defmethod sigfig-equal ((data1 float) (data2 float) &optional
                         (significant-figures *significant-figures*))
  "Return true if the floating point numbers have equal significant
figures."
  (%sigfig-equal data1 data2 significant-figures))

(defun %seq-sigfig-equal (seq1 seq2 significant-figures)
  "Return true if the element-wise comparison is equal to the
specified significant figures."
  (or
   (and (null seq1) (null seq2))
   (when (= (length seq1) (length seq2))
     (every
      (lambda (d1 d2) (sigfig-equal d1 d2 significant-figures))
      seq1 seq2))))

(defmethod sigfig-equal ((data1 list) (data2 list) &optional
                         (significant-figures *significant-figures*))
  "Return true if the lists are equal in length and the element-wise
comparison is equal to significant figures."
  (%seq-sigfig-equal data1 data2 significant-figures))

(defmethod sigfig-equal ((data1 vector) (data2 list) &optional
                         (significant-figures *significant-figures*))
  "Return true if the vector and the list are equal in length and the
element-wise comparison is equal to significant figures."
  (%seq-sigfig-equal data1 data2 significant-figures))

(defmethod sigfig-equal ((data1 list) (data2 vector) &optional
                         (significant-figures *significant-figures*))
  "Return true if the list and the vector are equal in length and the
element-wise comparison is equal to significant figures."
  (%seq-sigfig-equal data1 data2 significant-figures))

(defmethod sigfig-equal ((data1 vector) (data2 vector) &optional
                         (significant-figures *significant-figures*))
  "Return true if the vectors are equal in length and the element-wise
comparison is equal to significant figures."
  (%seq-sigfig-equal data1 data2 significant-figures))

(defmethod sigfig-equal ((data1 array) (data2 array) &optional
                         (significant-figures *significant-figures*))
  "Return true if the arrays are equal in length and the element-wise
comparison is equal to significant figures."
  (when (equal (array-dimensions data1)
               (array-dimensions data2))
    (%seq-sigfig-equal
     (make-array (array-total-size data1)
                 :element-type (array-element-type data1)
                 :displaced-to data1)
     (make-array (array-total-size data2)
                 :element-type (array-element-type data2)
                 :displaced-to data2)
     significant-figures)))
