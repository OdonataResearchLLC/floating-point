;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
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

|#

(in-package :asdf)

(defsystem :floating-point
  :description "Floating point functions."
  :version "0.1.0"
  :author "Thomas M. Hermann <thomas.m.hermann@odonata-research.com>"
  :license "MIT"
  :components
  ((:file "floating-point")
   (:file "constants" :depends-on ("floating-point"))
   (:file "error-analysis" :depends-on ("floating-point"))
   (:file "predicates" :depends-on ("error-analysis"))))

(defmethod perform :after
  ((operation load-op) (system (eql (find-system :floating-point))))
  "Update *FEATURES* if the system loads successfully."
  (pushnew :floating-point common-lisp:*features*))
