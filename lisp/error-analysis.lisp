#|

 Floating Point Functions

 Copyright (c) 2009-2012, Odonata Research LLC
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

(defvar *measure* 1
  "The default measure of the norm.")

(defgeneric default-epsilon (value)
  (:documentation
   "Return the default epsilon for the value."))

(defgeneric relative-error (exact approximate)
  (:documentation
   "Return the relative-error between the 2 quantities."))

(defgeneric %sumsq (data)
  (:documentation
   "Return the scaling parameter and the sum of the squares of the ~
    data."))

(defgeneric %sump (data p)
  (:documentation
   "Return the scaling parameter and the sum of the powers of p of the ~
    data."))

(defgeneric %norm (data &optional measure)
  (:documentation
   "Return the element-wise norm of the data."))

(defgeneric relative-error-norm (exact approximate &optional measure)
  (:documentation
   "Return the relative error norm "))

;;; (DEFAULT-EPSILON value) => epsilon
(defmethod default-epsilon ((value float))
  "Return a default epsilon value based on the floating point type."
  (typecase value
    (short-float  (* 2S0 short-float-epsilon))
    (single-float (* 2F0 single-float-epsilon))
    (double-float (* 2D0 double-float-epsilon))
    (long-float   (* 2L0 long-float-epsilon))))

(defmethod default-epsilon ((value complex))
  "Return a default epsilon value based on the complex type."
  (typecase value
    ((complex short-float)  (* 2S0 short-float-epsilon))
    ((complex single-float) (* 2F0 single-float-epsilon))
    ((complex double-float) (* 2D0 double-float-epsilon))
    ((complex long-float)   (* 2L0 long-float-epsilon))
    (t 0)))

;;; FIXME : Use the LOOP
(defmethod default-epsilon ((value list))
  "Return the default epsilon based on contents of the list."
  (reduce (lambda (x y) (max x (default-epsilon y)))
          value :initial-value 0))

;;; FIXME : Use the LOOP
(defmethod default-epsilon ((value vector))
  "Return the default epsilon based on the contents of the vector."
  (reduce (lambda (x y) (max x (default-epsilon y)))
          value :initial-value 0))

;;; FIXME : Use the LOOP
(defmethod default-epsilon ((value array))
  "Return the default epsilon based on the contents of the array."
  (reduce (lambda (x y) (max x (default-epsilon y)))
          (make-array (array-total-size value)
                      :element-type (array-element-type value)
                      :displaced-to value)
          :initial-value 0))

#|
  (RELATIVE-ERROR x y) => float
  [NumAlgoC] : Definition 1.3, pg. 2
               modified with Definition 1.1, pg. 1

  The definition of relative error in this routine is modified from
  the Definition 1.3 in [NumAlgoC] for cases when either the exact
  or the approximate value equals zero. According to Definition 1.3,
  the relative error is identically equal to 1 in those cases. This
  function returns the absolue error in those cases. This is more
  useful for testing.
|#

(defun %relative-error (exact approximate)
  "Return the relative error of the numbers."
  (abs (if (or (zerop exact) (zerop approximate))
	   (- exact approximate)
	   (/ (- exact approximate) exact))))

(defmethod relative-error ((exact float) (approximate float))
  "Return the error delta between the exact and approximate floating
point value."
  (%relative-error exact approximate))

(defmethod relative-error ((exact float) (approximate complex))
  "Return the relative error between the float and complex number."
  (%relative-error exact approximate))

(defmethod relative-error ((exact complex) (approximate float))
  "Return the relative error between the float and complex number."
  (%relative-error exact approximate))

(defmethod relative-error ((exact complex) (approximate complex))
  "Return the relative error of the complex numbers."
  (if (or (typep exact '(complex float))
          (typep approximate '(complex float)))
      (%relative-error exact approximate)
      (error "Relative error is only applicable to complex values with ~
              floating point parts.")))

;;; (%SUMSQ data) => scale, sumsq
(defmethod %sumsq ((data list))
  "Return the scaling parameter and the sum of the squares of the ~
   list."
  (let ((scale 0) (sumsq 1)
        (abs-val nil))
    (dolist (elm data (values scale sumsq))
      (when (< 0 (setf abs-val (abs elm)))
        (if (< scale abs-val)
            (setf sumsq (1+ (* sumsq (expt (/ scale abs-val) 2)))
                  scale abs-val)
            (incf sumsq (expt (/ elm scale) 2)))))))

(defmethod %sumsq ((data vector))
  "Return the scaling parameter and the sum of the squares of the ~
   vector."
  (let ((scale 0) (sumsq 1)
        (size (length data))
        (abs-val nil))
    (dotimes (index size (values scale sumsq))
      (when (< 0 (setf abs-val (abs (svref data index))))
        (if (< scale abs-val)
            (setf sumsq (1+ (* sumsq (expt (/ scale abs-val) 2)))
                  scale abs-val)
            (incf sumsq (expt (/ (svref data index) scale) 2)))))))

(defmethod %sumsq ((data array))
  "Return the scaling parameter and the sum of the squares of the ~
   array."
  (%sumsq (make-array (array-total-size data)
                      :element-type (array-element-type data)
                      :displaced-to data)))

;;; (SUMP data) => scale, sump
(defmethod %sump ((data list) (p real))
  "Return the scaling parameter and the sum of the powers of p of the ~
   data."
  (let ((scale 0) (sump 1)
        (abs-val nil))
    (dolist (elm data (values scale sump))
      (when (< 0 (setf abs-val (abs elm)))
        (if (< scale abs-val)
            (setf sump  (1+ (* sump (expt (/ scale abs-val) p)))
                  scale abs-val)
            (incf sump (expt (/ elm scale) p)))))))

(defmethod %sump ((data vector) (p real))
  "Return the scaling parameter and the sum of the powers of p of the ~
   vector."
  (let ((scale 0) (sump 1)
        (size (length data))
        (abs-val nil))
    (dotimes (index size (values scale sump))
      (when (< 0 (setf abs-val (abs (svref data index))))
        (if (< scale abs-val)
            (setf sump  (1+ (* sump (expt (/ scale abs-val) p)))
                  scale abs-val)
            (incf sump (expt (/ (svref data index) scale) p)))))))

(defmethod %sump ((data array) (p real))
  "Return the scaling parameter and the sum of the powers of p of the ~
   array."
  (%sump (make-array (array-total-size data)
                     :element-type (array-element-type data)
                     :displaced-to data)
         p))

;;; (NORM data) => float
(defun %seq-1-norm (data)
  "Return the Taxicab norm of the sequence."
  ;; FIXME : Use the LOOP
  (reduce (lambda (x y) (+ x (abs y)))
          data :initial-value 0))

(defun %seq-2-norm (data)
  "Return the Euclidean norm of the sequence."
  (multiple-value-bind (scale sumsq)
      (%sumsq (map-into (make-array (length data)) #'abs data))
    (* scale (sqrt sumsq))))

(defun %seq-p-norm (data p)
  "Return the p norm of the sequence."
  (multiple-value-bind (scale sump)
      (%sump (map-into (make-array (length data)) #'abs data) p)
    (* scale (expt sump (/ p)))))

(defun %seq-inf-norm (data)
  "Return the infinity, or maximum, norm of the sequence."
  ;; FIXME : Use the LOOP
  (reduce (lambda (x y) (max x (abs y)))
          data :initial-value 0))

(defun %seq-norm (data measure)
  "Return the norm of the sequence according to the measure."
  (cond
    ((equalp measure 1)
     (%seq-1-norm data))
    ((equalp measure 2)
     (%seq-2-norm data))
    ((numberp measure)
     (%seq-p-norm data measure))
    ((equalp measure :infinity)
     (%seq-inf-norm data))
    (t (error "Unrecognized norm, ~A." measure))))

(defmethod %norm ((data list) &optional (measure *measure*))
  "Return the norm of the list according to the measure."
  (%seq-norm data measure))

(defmethod %norm ((data vector) &optional (measure *measure*))
  "Return the norm of the vector according to the measure."
  (%seq-norm data measure))

(defmethod %norm ((data array) &optional (measure *measure*))
  "Return the entrywise norm of the array according to the measure."
  (let ((flat-data (make-array (array-total-size data)
                               :element-type (array-element-type data)
                               :displaced-to data)))
    (cond
      ((and (numberp measure) (< 0 measure))
       (warn "Measure ~D results in an entrywise p-norm." measure)
       (%seq-p-norm flat-data measure))
      ((equalp measure :frobenius)
       (%seq-2-norm flat-data))
      ((equalp measure :max)
       (%seq-inf-norm flat-data))
      (t (error "Unrecognized norm, ~A." measure)))))

;;; (RELATIVE-ERROR-NORM exact approximate measure) => float
(defun %relative-error-norm (exact approximate measure)
  "Return the relative error norm of the sequences."
  (/ (%norm (map-into (make-array (length exact))
                      (lambda (x1 x2) (abs (- x1 x2)))
                      exact approximate) measure)
     (%norm exact measure)))

(defmethod relative-error-norm ((exact list) (approximate list)
                                &optional (measure *measure*))
  "Return the relative error norm of the lists."
  (if (= (length exact) (length approximate))
      (%relative-error-norm exact approximate measure)
      (error "Lists are not equal in length.")))

(defmethod relative-error-norm ((exact list) (approximate vector)
                                &optional (measure *measure*))
  "Return the relative error norm of the list and the vector."
  (if (= (length exact) (length approximate))
      (%relative-error-norm exact approximate measure)
      (error "The list and vector are not equal in length.")))

(defmethod relative-error-norm ((exact vector) (approximate list)
                                &optional (measure *measure*))
  "Return the relative error norm of the list and the vector."
  (if (= (length exact) (length approximate))
      (%relative-error-norm exact approximate measure)
      (error "The list and vector are not equal in length.")))

(defmethod relative-error-norm ((exact vector) (approximate vector)
                                &optional (measure *measure*))
  "Return the relative error norm of the vectors."
  (if (= (length exact) (length approximate))
      (%relative-error-norm exact approximate measure)
      (error "Vectors are not equal in length.")))

(defmethod relative-error-norm ((exact array) (approximate vector)
                                &optional (measure *measure*))
  "Return the relative error norm of the arrays."
  (if (equal (array-dimensions exact)
             (array-dimensions approximate))
      (%relative-error-norm
       (make-array (array-total-size exact)
                   :element-type (array-element-type exact)
                   :displaced-to exact)
       (make-array (array-total-size approximate)
                   :element-type (array-element-type approximate)
                   :displaced-to approximate)
       measure)
      (error "Arrays are not equal dimensions.")))
