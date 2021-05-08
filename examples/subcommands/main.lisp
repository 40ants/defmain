(defpackage #:example
  (:use #:cl)
  (:export #:main))
(in-package example)


(defmain:defmain (main :program-name "example")
    ((verbose "Show more details in the output."
              :flag t)
     &subcommand)
  "The main program to host subcommands.")


(defmain:defcommand (main upload)
    ((upstream "Repository name")
     (force "Rewrite changes in case of conflict"
            :flag t))
  "Upload data to upstream"

  (unless upstream
    (error "--upstream is required"))

  (if verbose
      (format t "Uploading to ~S upstream with force = ~S~%" 
              upstream
              force)
      (format t "Uploading data.%")))
