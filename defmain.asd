#|
  This file is a part of defmain project.
|#


(in-package :cl-user)
(defpackage defmain-asd
  (:use :cl :asdf))
(in-package :defmain-asd)


(defsystem defmain
  :version (:read-file-form "version.lisp-expr")
  :author ""
  :license ""
  :depends-on (net.didierverna.clon
               cl-strings
               alexandria)
  :components ((:module "src"
                :components
                ((:file "defmain"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.rst"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq)
                (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op defmain-test))))

