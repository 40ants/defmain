#|
  This file is a part of defmain project.
|#

(in-package :cl-user)
(defpackage defmain-test-asd
  (:use :cl :asdf))
(in-package :defmain-test-asd)

(defsystem defmain-test
  :author ""
  :license ""
  :depends-on (:defmain
               :prove
               :hamcrest-prove)
  :components ((:module "t"
                :components
                ((:test-file "defmain"))))
  :description "Test system for defmain"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
