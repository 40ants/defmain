(defsystem "example"
  :depends-on ("defmain")
  :components ((:file "main"))
  :entry-point "example:main"
  :build-operation "program-op")
