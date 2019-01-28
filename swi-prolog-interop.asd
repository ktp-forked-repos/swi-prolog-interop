
(defpackage swi-prolog-interop-system
  (:use :common-lisp :asdf))

(in-package :swi-prolog-interop-system)

(defsystem "swi-prolog-interop"
  :description "Provides basic communication between SBCL and SWI-Prolog (swipl)"
  :depends-on ("cl-ppcre")
  :version "0.1"
  :author "Ernest Deák <gordon.zar@gmail.com>"
  :license "BSD 2-Clause License"
  :components ((:file "swi-prolog-interop")))
