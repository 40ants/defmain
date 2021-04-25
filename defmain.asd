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
  :depends-on ("defmain/defmain")
  :in-order-to ((test-op (test-op defmain-test))))

