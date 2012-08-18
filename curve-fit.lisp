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

|#

(in-package :floating-point)

;;; FIXME : Needs to be audited for performance. This could be easily
;;;         modified to calculate the accumulated values in parallel.
;;; FIXME : Should perform scaled summations.
;;; FIXME : Check for zero slope.
(defun linear-least-squares (data-x data-y)
  "Return the slope, intercept, variance and r^2.

'Engineering Mathematics and Statistics' Cheremisinoff, pg. 126"
  (if (= (length data-x) (length data-y))
      (loop with data-N = (length data-x)
            for x across data-x
            and y across data-y
            ;; Sum data quantities
            sum x into sum-x
            sum y into sum-y
            sum (* x x) into sum-x2
            sum (* y y) into sum-y2
            sum (* x y) into sum-xy
            ;; Results
            finally
            (let* ((avg-x (/ sum-x data-N))
                   (avg-y (/ sum-y data-N))
                   (sxx (- sum-x2 (* sum-x avg-x)))
                   (syy (- sum-y2 (* sum-y avg-y)))
                   (sxy (- sum-xy (* sum-x avg-y)))
                   ;; Results
                   (slope     (/ sxy sxx))
                   (intercept (- avg-y (* slope avg-x)))
                   (variance  (/ (- syy (* slope slope sxx)) (- data-N 2)))
                   (correlation (/ (* sxy sxy) sxx syy)))
              ;; Return multiple values to reduce consing.
              (return-from linear-least-squares
                (values slope intercept variance correlation))))
      (error "X and Y data must be equal length.")))
