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
                    (symbol-call :prove-asdf :run-test-system c)
                    (clear-system c)))
