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

(defun arithmetic-mean (data1 data2 &rest data)
  "Return the arithmetic mean of the data."
  (if data
      (loop for item in data sum item into sumdata
            finally
            (return
             (/ (+ data1 data2 sumdata)
                (+ 2 (length data)))))
      (* 1/2 (+ data1 data2))))

(defun standard-deviation (data1 data2 &rest data)
  "Return the standard deviation of the data."
  (if data
      (loop with mean = (apply 'arithmetic-mean data1 data2 data)
            with diff1 = (* (- data1 mean) (- data1 mean))
            with diff2 = (* (- data2 mean) (- data2 mean))
            for item in data
            as diff = (- item mean)
            sum (* diff diff) into sumdiff
            finally
            (return
             (sqrt (/ (+ diff1 diff2 sumdiff)
                      (+ 2 (length data))))))
      (let* ((mean (arithmetic-mean data1 data2))
             (diff1 (* (- data1 mean) (- data1 mean)))
             (diff2 (* (- data2 mean) (- data2 mean))))
        (sqrt (* 1/2 (+ diff1 diff2))))))

(defun sample-standard-deviation (data1 data2 &rest data)
  "Return the sample standard deviation of the data."
  (if data
      (loop with mean = (apply 'arithmetic-mean data1 data2 data)
            with diff1 = (* (- data1 mean) (- data1 mean))
            with diff2 = (* (- data2 mean) (- data2 mean))
            for item in data
            as diff = (- item mean)
            sum (* diff diff) into sumdiff
            finally
            (return
             (sqrt (/ (+ diff1 diff2 sumdiff)
                      (+ 1 (length data))))))
      (let* ((mean (arithmetic-mean data1 data2))
             (diff1 (* (- data1 mean) (- data1 mean)))
             (diff2 (* (- data2 mean) (- data2 mean))))
        (sqrt (+ diff1 diff2)))))

(defun mean-and-standard-deviation (data1 data2 &rest data)
  "Return the arithmetic mean and sample standard deviation."
  (if data
      (loop with mean = (apply 'arithmetic-mean data1 data2 data)
            with diff1 = (* (- data1 mean) (- data1 mean))
            with diff2 = (* (- data2 mean) (- data2 mean))
            for item in data
            as diff = (- item mean)
            sum (* diff diff) into sumdiff
            finally
            (return
             (values mean (sqrt (/ (+ diff1 diff2 sumdiff)
                                   (+ 1 (length data)))))))
      (let* ((mean (arithmetic-mean data1 data2))
             (diff1 (* (- data1 mean) (- data1 mean)))
             (diff2 (* (- data2 mean) (- data2 mean))))
        (values mean (sqrt (+ diff1 diff2))))))
