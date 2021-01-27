(defsystem defmain
  :version (:read-file-form "version.lisp-expr")
  :author "Alexander Artemenko"
  :license "BSD"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("defmain/defmain")
  :description "A wrapper around net.didierverna.clon which makes command line arguments parsing easier."
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

;; To not get rid of banners like this, we need to load only core
;; system of Clon:
;; 
;; *******************************************************************
;; * WARNING: the CC environment variable is not set.                *
;; * Clon will be loaded without support for terminal autodetection. *
;; * See sections 2 and A.1 of the user manual for more information. *
;; *******************************************************************
;;
;;  This system does not try to use non-ANSI features of Clon.
(asdf:register-system-packages "net.didierverna.clon.core"
                               '(#:net.didierverna.clon))
