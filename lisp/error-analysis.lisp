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

;;; Interface

(defgeneric default-epsilon (value)
  (:documentation
   "Return the default epsilon for the value."))

(defgeneric relative-error (exact approximate)
  (:documentation
   "Return the relative-error between the 2 quantities."))

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

(defmethod default-epsilon ((value list))
  "Return the default epsilon for the list."
  (loop for val in value maximize (default-epsilon val)))

(defmethod default-epsilon ((value vector))
  (loop for val across value maximize (default-epsilon val)))

(defmethod default-epsilon ((value array))
  (loop for val across
        (make-array
         (array-total-size value)
         :element-type (array-element-type value)
         :displaced-to value)
        maximize (default-epsilon val)))

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
  (abs
   (if (or (zerop exact) (zerop approximate))
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
      (error
       "Relative error is only applicable to (COMPLEX FLOAT).")))
