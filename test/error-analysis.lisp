#|

 Floating Point Unit Testing

 Copyright (c) 2012, Odonata Research LLC
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
   o  Neither the name of the Odonata Research LLC nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

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

(define-test default-epsilon-list
  (assert-eql
   (* 2S0 short-float-epsilon)
   (fp:default-epsilon (list 1S0 1F0 1D0 1L0)))
  (assert-eql
   (* 2F0 single-float-epsilon)
   (fp:default-epsilon (list 1F0 1F0 1D0 1L0)))
  (assert-eql
   (* 2D0 double-float-epsilon)
   (fp:default-epsilon (list 1D0 1D0 1D0 1L0)))
  (assert-eql
   (* 2L0 long-float-epsilon)
   (fp:default-epsilon (list 1L0 1L0 1L0 1L0))))

(define-test default-epsilon-vector
  (assert-eql
   (* 2S0 short-float-epsilon)
   (fp:default-epsilon (vector 1S0 1F0 1D0 1L0)))
  (assert-eql
   (* 2F0 single-float-epsilon)
   (fp:default-epsilon (vector 1F0 1F0 1D0 1L0)))
  (assert-eql
   (* 2D0 double-float-epsilon)
   (fp:default-epsilon (vector 1D0 1D0 1D0 1L0)))
  (assert-eql
   (* 2L0 long-float-epsilon)
   (fp:default-epsilon (vector 1L0 1L0 1L0 1L0))))

(define-test default-epsilon-array
  (assert-eql
   (* 2S0 short-float-epsilon)
   (fp:default-epsilon
    (make-array
     '(4 2 2) :initial-contents
     '(((1S0 2S0) (3S0 4S0))
       ((1F0 2F0) (3F0 4F0))
       ((1D0 2D0) (3D0 4D0))
       ((1L0 2L0) (3L0 4L0))))))
  (assert-eql
   (* 2F0 single-float-epsilon)
   (fp:default-epsilon
    (make-array
     '(4 2 2) :initial-contents
     '(((1F0 2F0) (3F0 4F0))
       ((1F0 2F0) (3F0 4F0))
       ((1D0 2D0) (3D0 4D0))
       ((1L0 2L0) (3L0 4L0))))))
  (assert-eql
   (* 2D0 double-float-epsilon)
   (fp:default-epsilon
    (make-array
     '(4 2 2) :initial-contents
     '(((1D0 2D0) (3D0 4D0))
       ((1D0 2D0) (3D0 4D0))
       ((1D0 2D0) (3D0 4D0))
       ((1L0 2L0) (3L0 4L0))))))
  (assert-eql
   (* 2L0 long-float-epsilon)
   (fp:default-epsilon
    (make-array
     '(4 2 2) :initial-contents
     '(((1L0 2L0) (3L0 4L0))
       ((1L0 2L0) (3L0 4L0))
       ((1L0 2L0) (3L0 4L0))
       ((1L0 2L0) (3L0 4L0)))))))

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
