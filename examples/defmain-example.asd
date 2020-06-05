(defsystem "defmain-example"
  :depends-on ("defmain")
  :components ((:file "main"))
  :entry-point "defmain-example:main"
  :build-operation "program-op")
