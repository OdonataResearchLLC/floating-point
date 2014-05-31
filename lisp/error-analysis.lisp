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
