#|

  Floating Point Unit Testing

  Copyright (c) 2012-2014, Odonata Research LLC

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

|#

(in-package :floating-point-test)

;;; default-epsilon tests

(define-test default-epsilon
  (assert-eql
   (* 2S0 short-float-epsilon)
   (fp:default-epsilon 1S0))
  (assert-eql
   (* 2F0 single-float-epsilon)
   (fp:default-epsilon 1F0))
  (assert-eql
   (* 2D0 double-float-epsilon)
   (fp:default-epsilon 1D0))
  (assert-eql
   (* 2L0 long-float-epsilon)
   (fp:default-epsilon 1L0)))

(define-test default-epsilon-complex
  (assert-eql
   (* 2S0 short-float-epsilon)
   (fp:default-epsilon (complex 1S0 1S0)))
  (assert-eql
   (* 2F0 single-float-epsilon)
   (fp:default-epsilon (complex 1F0 1F0)))
  (assert-eql
   (* 2D0 double-float-epsilon)
   (fp:default-epsilon (complex 1D0 1D0)))
  (assert-eql
   (* 2L0 long-float-epsilon)
   (fp:default-epsilon (complex 1L0 1L0))))

;;; relative-error tests

(define-test %relative-error
  ;; Fundamental tests
  (assert-eql 1D0 (fp::%relative-error 0D0 1D0))
  (assert-eql 1D0 (fp::%relative-error 1D0 0D0))
  (assert-true
   (> (* 2D0 double-float-epsilon)
      (fp::%relative-error
       1D0 (+ 1D0 double-float-epsilon))))
  (assert-true
   (> (* 2D0 double-float-epsilon)
      (fp::%relative-error
       (+ 1D0 double-float-epsilon) 1D0)))
  (assert-true
   (> (* 2D0 double-float-epsilon)
      (fp::%relative-error
       1D0 (- 1D0 double-float-epsilon))))
  (assert-true
   (> (* 2D0 double-float-epsilon)
      (fp::%relative-error
       (- 1D0 double-float-epsilon) 1D0))))

(define-test relative-error-float
  (assert-true 
   (> (* 2F0 single-float-epsilon)
      (fp:relative-error 1F0 (+ 1F0 single-float-epsilon))))
  (assert-true 
   (> (* 2F0 single-float-epsilon)
      (fp:relative-error (+ 1F0 single-float-epsilon) 1F0)))
  (assert-true
   (> (* 2F0 single-float-epsilon)
      (fp:relative-error 1F0 (+ 1D0 single-float-epsilon))))
  (assert-true
   (> (* 2F0 single-float-epsilon)
      (fp:relative-error (+ 1D0 single-float-epsilon) 1F0)))
  (assert-true
   (> (* 2F0 single-float-epsilon)
      (fp:relative-error 1F0 (- 1D0 single-float-epsilon))))
  (assert-true
   (> (* 2F0 single-float-epsilon)
      (fp:relative-error (- 1D0 single-float-epsilon) 1F0)))
  (assert-true
   (> (* 2D0 double-float-epsilon)
      (fp:relative-error 1F0 (+ 1D0 double-float-epsilon))))
  (assert-true
   (> (* 2D0 double-float-epsilon)
      (fp:relative-error (+ 1D0 double-float-epsilon) 1F0)))
  (assert-true
   (> (* 2D0 double-float-epsilon)
      (fp:relative-error 1F0 (- 1D0 double-float-epsilon))))
  (assert-true
   (> (* 2D0 double-float-epsilon)
      (fp:relative-error (- 1D0 double-float-epsilon) 1F0)))
  (assert-true
   (> (* 2D0 double-float-epsilon)
      (fp:relative-error 1D0 (+ 1D0 double-float-epsilon))))
  (assert-true
   (> (* 2D0 double-float-epsilon)
      (fp:relative-error (+ 1D0 double-float-epsilon) 1D0)))
  (assert-true
   (> (* 2D0 double-float-epsilon)
      (fp:relative-error 1D0 (- 1D0 double-float-epsilon))))
  (assert-true
   (> (* 2D0 double-float-epsilon)
      (fp:relative-error (- 1D0 double-float-epsilon) 1D0))))

(define-test relative-error-complex
  (assert-error
   'error
   (fp:relative-error
    (complex 1 1) (complex 2 2)))
  (assert-true
   (> (* 2F0 single-float-epsilon)
      (fp:relative-error
       (complex 1F0 1F0)
       (complex (+ 1F0 single-float-epsilon)
                (+ 1F0 single-float-epsilon)))))
  (assert-true
   (> (* 2F0 single-float-epsilon)
      (fp:relative-error
       (complex 1F0 1F0)
       (complex (- 1F0 single-float-epsilon)
                (+ 1F0 single-float-epsilon)))))
  (assert-true
   (> (* 2F0 single-float-epsilon)
      (fp:relative-error
       (complex 1F0 1F0)
       (complex (+ 1F0 single-float-epsilon)
                (- 1F0 single-float-epsilon)))))
  (assert-true
   (> (* 2F0 single-float-epsilon)
      (fp:relative-error
       (complex 1F0 1F0)
       (complex (- 1F0 single-float-epsilon)
                (- 1F0 single-float-epsilon)))))
  (assert-true
   (> (* 2F0 single-float-epsilon)
      (fp:relative-error
       (complex (+ 1F0 single-float-epsilon)
                (+ 1F0 single-float-epsilon))
       (complex 1F0 1F0))))
  (assert-true
   (> (* 2F0 single-float-epsilon)
      (fp:relative-error
       (complex (- 1F0 single-float-epsilon)
                (+ 1F0 single-float-epsilon))
       (complex 1F0 1F0))))
  (assert-true
   (> (* 2F0 single-float-epsilon)
      (fp:relative-error
       (complex (+ 1F0 single-float-epsilon)
                (- 1F0 single-float-epsilon))
       (complex 1F0 1F0))))
  (assert-true
   (> (* 2F0 single-float-epsilon)
      (fp:relative-error
       (complex (- 1F0 single-float-epsilon)
                (- 1F0 single-float-epsilon))
       (complex 1F0 1F0))))
  (assert-true
   (> (* 2D0 double-float-epsilon)
      (fp:relative-error
       (complex 1D0 1D0)
       (complex (+ 1D0 double-float-epsilon)
                (+ 1D0 double-float-epsilon)))))
  (assert-true
   (> (* 2D0 double-float-epsilon)
      (fp:relative-error
       (complex 1D0 1D0)
       (complex (- 1D0 double-float-epsilon)
                (+ 1D0 double-float-epsilon)))))
  (assert-true
   (> (* 2D0 double-float-epsilon)
      (fp:relative-error
       (complex 1D0 1D0)
       (complex (+ 1D0 double-float-epsilon)
                (- 1D0 double-float-epsilon)))))
  (assert-true
   (> (* 2D0 double-float-epsilon)
      (fp:relative-error
       (complex 1D0 1D0)
       (complex (- 1D0 double-float-epsilon)
                (- 1D0 double-float-epsilon)))))
  (assert-true
   (> (* 2D0 double-float-epsilon)
      (fp:relative-error
       (complex (+ 1D0 double-float-epsilon)
                (+ 1D0 double-float-epsilon))
       (complex 1D0 1D0))))
  (assert-true
   (> (* 2D0 double-float-epsilon)
      (fp:relative-error
       (complex (- 1D0 double-float-epsilon)
                (+ 1D0 double-float-epsilon))
       (complex 1D0 1D0))))
  (assert-true
   (> (* 2D0 double-float-epsilon)
      (fp:relative-error
       (complex (+ 1D0 double-float-epsilon)
                (- 1D0 double-float-epsilon))
       (complex 1D0 1D0))))
  (assert-true
   (> (* 2D0 double-float-epsilon)
      (fp:relative-error
       (complex (- 1D0 double-float-epsilon)
                (- 1D0 double-float-epsilon))
       (complex 1D0 1D0)))))
