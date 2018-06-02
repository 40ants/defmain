(defsystem defmain-test
  :description "Test system for defmain"
  :author "Alexander Artemenko"
  :class :package-inferred-system
  :pathname "t"
  :depends-on (:defmain
               "defmain-test/defmain")
  :perform (test-op :after (op c)
                    (symbol-call :rove :run c)))
