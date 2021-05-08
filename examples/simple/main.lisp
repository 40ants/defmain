(defpackage #:example
  (:use #:cl)
  (:export #:main))
(in-package example)


(defmain:defmain main
    ((debug "Show traceback instead of short message."
            :flag t)
     (log   "Filename to write log to.")
     (token "GitHub personal access token."
            :env-var "TOKEN")
     &rest repositories)
  "Utility to analyze github forks."
  
  (format t
          "Repositories: ~{~S~^, ~}~%~
           Debug: ~S~%~
           Log: ~S~%~
           Token: ~S~%"
          repositories
          debug
          log
          token))
