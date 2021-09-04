(defsystem defmain
  :version (:read-file-form "version.lisp-expr")
  :author "Alexander Artemenko"
  :license "BSD"
  :description "A wrapper around net.didierverna.clon which makes command line arguments parsing easier."
  :homepage "https://40ants.com/defmain"
  :bug-tracker "https://github.com/40ants/defmain/issues"
  :source-control (:git "https://github.com/40ants/defmain")
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("defmain/defmain"
               "defmain/changelog")
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
