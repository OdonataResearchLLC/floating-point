#|

  Floating Point Functions

  Copyright (c) 2009-2014, Odonata Research LLC

  Permission is hereby granted, free  of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction,  including without limitation the rights
  to use, copy, modify,  merge,  publish,  distribute,  sublicense, and/or sell
  copies of the  Software,  and  to  permit  persons  to  whom  the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and  this  permission  notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED  "AS IS",  WITHOUT  WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT  NOT  LIMITED  TO  THE  WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE  AND  NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT  HOLDERS  BE  LIABLE  FOR  ANY  CLAIM,  DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

  References
  [NumAlgoC] Gisela Engeln-Mullges and Frank Uhlig "Numerical
             Algorithms with C", Springer, 1996
             ISBN: 3-540-60530-4

|#

(in-package :floating-point)

;;; Interface

(defvar *epsilon* nil
  "The default error epsilon.")

(defvar *significant-figures* 4
  "The default number of significant figures.")

(defgeneric float-equal (data1 data2 &optional epsilon)
  (:documentation
   "Return true if the floating point data is equal."))

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

(defmethod float-equal
    ((data1 float) (data2 float) &optional (epsilon *epsilon*))
  "Return true if the relative error between data1 and data2 is less
than epsilon."
  (%float-equal
   data1 data2
   (or epsilon
       (max (default-epsilon data1)
            (default-epsilon data2)))))

(defmethod float-equal
    ((data1 float) (data2 rational) &optional (epsilon *epsilon*))
  "Return true if the relative error between data1 and data2 is less
than epsilon."
  (%float-equal
   data1 (float data2 data1)
   (or epsilon (default-epsilon data1))))

(defmethod float-equal
    ((data1 rational) (data2 float) &optional (epsilon *epsilon*))
  "Return true if the relative error between data1 and data2 is less
than epsilon."
  (%float-equal
   (float data1 data2) data2
   (or epsilon (default-epsilon data2))))

(defmethod float-equal
    ((data1 float) (data2 complex) &optional (epsilon *epsilon*))
  "Return true if the relative error between data1 and data2 is less
than epsilon."
  (%float-equal
   data1 data2
   (or epsilon
       (max (default-epsilon data1)
            (default-epsilon data2)))))

(defmethod float-equal
    ((data1 complex) (data2 float) &optional (epsilon *epsilon*))
  "Return true if the relative error between data1 and data2 is less
than epsilon."
  (%float-equal
   data1 data2
   (or epsilon
       (max (default-epsilon data1)
            (default-epsilon data2)))))

(defmethod float-equal
    ((data1 complex) (data2 complex) &optional (epsilon *epsilon*))
  "Return true if the relative error between data1 and data2 is less
than epsilon."
  (< (relative-error data1 data2)
     (or epsilon (max (default-epsilon data1)
                      (default-epsilon data2)))))

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

;;; (SIGFIG-EQUAL data1 data2 significant-figures) => true or false
(defmethod sigfig-equal
    ((data1 float) (data2 float) &optional
     (significant-figures *significant-figures*))
  "Return true if the floating point numbers have equal significant
figures."
  (if (or (zerop data1) (zerop data2))
      (< (abs (+ data1 data2))
         (* 5D-1 (expt 1D1 (- significant-figures))))
      (multiple-value-bind (sig1 exp1) (%normalize-float data1)
        (multiple-value-bind (sig2 exp2) (%normalize-float data2)
          (=
           (round (* sig1 (expt 1D1 significant-figures)))
           (round (* sig2 (expt 1D1 (- significant-figures (- exp1 exp2))))))))))
