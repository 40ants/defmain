(defpackage defmain/defmain
  (:use :cl)
  (:nicknames :defmain)
  (:import-from #:net.didierverna.clon
                #:remainder
                #:defsynopsis
                #:make-context
                #:help
                #:getopt)
  (:import-from #:alexandria
                #:ensure-symbol)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:cl-strings
                #:join
                #:split
                #:starts-with)
  (:import-from #:cl-inflector
                #:singular-of)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  (:export #:defmain
           #:print-help
           #:subcommand
           #:defcommand
           #:print-commands-help
           #:get-subcommand-name))
(in-package :defmain)

(in-readtable pythonic-string-syntax)


(defsection @index (:title "DEFMAIN - intuitive command line options parser for Common Lisp")
  (defmain system)
  (@reasoning section)
  (@installation section)
  (@roadmap section))


(defsection @reasoning (:title "Reasoning")
  """
Library [`net.didierverna.clon`](https://github.com/didierverna/clon)
very powerful, but too complicated to use in simple cases. This library
provides a wrapper which will suite your needs in 80% cases.

Compare this code, which uses DEFMAIN macro:

```
(defmain main ((debug "Show traceback instead of short message."
                      :flag t)
               (log   "Filename to write log to.")
               (token "GitHub personal access token."
                      :env-var "TOKEN")
               &rest repositories)
  "Utility to analyze github forks."

   ;; Making real work
  (loop for reporitory in repositories
        do (analyze repository
                    :log log
                    :debug debug
                    :token token)))
```

With code providing the same functionality, but using raw
`net.didierverna.clon` system:

```
(net.didierverna.clon:defsynopsis (:postfix "REPOSITORY")
  (text :contents "This utility builds a report about all non-merged commits for any github repository. Just give some repository name like \"antirez/redis\" as an argument and pipe stdout to some file.")
  (flag :short-name "h" :long-name "help"
        :description "Print this help and exit.")
  (flag :short-name "v" :long-name "version"
        :description "Print version number and exit.")
  (flag :long-name "debug"
        :description "Show traceback instead of short message.")
  (stropt :short-name "l" :long-name "log"
          :description "Filename to write log to.")
  (stropt :short-name "t" :long-name "token"
          :env-var "TOKEN"
          :description "GitHub personal access token."))

(defun main (&rest argv)
  (declare (ignorable argv))
  (net.didierverna.clon:make-context :cmdline (cons "12forks" argv))
  (when (net.didierverna.clon:getopt :long-name "help")
    (net.didierverna.clon:help)
    (net.didierverna.clon:exit))

  ;; Making real work
  (loop for reporitory in (remainder)
        do (analyze repository
                    :log (net.didierverna.clon:getopt :long-name "log")
                    :debug (net.didierverna.clon:getopt :long-name "debug")
                    :token (net.didierverna.clon:getopt :long-name "token"))))
```
""")


(defsection @installation (:title "Installation")
  """
This system is available as part of the https://ultralisp.org distribution. Follow instruction
on the site to setup the distribution, and then install DEFMAIN system using Quicklisp client:

    (ql:quickload :defmain)

"""
  )


(defsection @roadmap (:title "Roadmap"
                      :ignore-words ("RUNNING"
                                     "*INVOKE-DEBUGGER-HOOK*"))
  """
* Make better support for integer arguments.
* Support more types of arguments, like filepathes and enums.
* Raise error when two short options are identical during
  macro-expansion, not during runtime. Right now the `clon`
  checks this during runtime:

      Unhandled SIMPLE-ERROR in thread #<SB-THREAD:THREAD "main thread"
      RUNNING {10005285B3}>:
    
      Options #<LISPOBJ {1002705593}> and #<STROPT {1002705C03}>:
      indentical short name "s".

      Backtrace for: #<SB-THREAD:THREAD "main thread" RUNNING
      {10005285B3}>
      0: (SB-DEBUG::DEBUGGER-DISABLED-HOOK #<SIMPLE-ERROR "Options ~A and
      ~A: indentical short name ~S." {100277D8F3}> #<unused argument>
      :QUIT T)
      1: (SB-DEBUG::RUN-HOOK SB-EXT:*INVOKE-DEBUGGER-HOOK* #<SIMPLE-ERROR
      "Options ~A and ~A: indentical short name ~S." {100277D8F3}>)
      2: (INVOKE-DEBUGGER #<SIMPLE-ERROR "Options ~A and ~A: indentical short name ~S." {100277D8F3}>)
      3: (ERROR "Options ~A and ~A: indentical short name ~S."
      #<NET.DIDIERVERNA.CLON::LISPOBJ {1002705593}>
      #<NET.DIDIERVERNA.CLON: :STROPT {1002705C03}> "s")
      4: ((:METHOD NET.DIDIERVERNA.CLON::CHECK-NAME-CLASH
      (NET.DIDIERVERNA.CLON::OPTION NET.DIDIERVERNA.CLON::OPTION))
      #<NET.DIDIERVERNA.CLON::LISPOBJ {1002705593}>
      #<NET.DIDIERVERNA.CLON::STROPT {1002705C03}>) [fast-method]
      5: ((:METHOD INITIALIZE-INSTANCE :AFTER
      (NET.DIDIERVERNA.CLON::CONTAINER)) #<NET.DIDIERVERNA.CLON::SYNOPSIS
      {100270C013}>) [fast-method]

"""
  )

;; For reference on defsynopsys, take a look at it's documentation
;; https://www.lrde.epita.fr/%7Edidier/software/lisp/clon/user/

(define-condition argument-is-required-error (error)
  ((argument-name :initarg :name
                  :reader get-argument-name))
  (:report (lambda (c stream)
             (format stream
                     "Argument ~S is required."
                     (string-downcase (symbol-name
                                       (get-argument-name c)))))))


(defun get-rest-arg (list)
  "Takes a lambda list and returns a symbol, naming &rest argument, or nil."
  (let ((rest-arg-position (position '&rest list)))
    (when rest-arg-position
      (nth (+ rest-arg-position 1)
           list))))


(defun is-has-subcommand (args)
  "Returns t if there is &subcommand symbol in the list."
  (loop for arg in args
        when (and (symbolp arg)
                  (string-equal arg '&subcommand))
          do (return t)))


(defun make-postfix-string (positional-args rest-arg)
  (check-type positional-args list)
  (check-type rest-arg (or symbol null))
  
  (join (append (mapcar #'symbol-name positional-args)
                (when rest-arg
                  (list (concatenate 'string
                                     (string-upcase (singular-of rest-arg))
                                     "..."))))
        :separator " "))


(defun make-synopsis-args (defmain-args)
  "Checks if there is &rest or &subcommand part in defmain's args and outputs it either as

   \(:postfix \"REPOSITORY\"\) list

   or as

   \(:postfix \"SUBCOMMAND\"\) list."

  ;; TODO: add a test
  (let ((rest-arg (get-rest-arg defmain-args))
        (positional-args (get-positional-args defmain-args))
        (has-subcommand (is-has-subcommand defmain-args)))
    
    (when (and rest-arg
               has-subcommand)
      (error "You can't use &rest and &subcommand simultaneously"))

    (cond
      ((or rest-arg
           positional-args)
       `(:postfix ,(make-postfix-string positional-args rest-arg)))
      (has-subcommand
       `(:postfix "COMMAND")))))


(defun make-long-name (symbol)
  (string-downcase (symbol-name symbol)))


(defun make-short-name (symbol)
  (subseq (make-long-name symbol)
          0 1))


(defun get-value-if-symbol (value)
  "If value is a bound symbol, then returns its bound value.
   For all other cases just returns a value itself."
  (cond
    ((and (typep value 'symbol)
          (boundp value))
     (symbol-value value))
    (t value)))


(defun make-field-description (name
                               documentation
                               &key
                                 ;; name of environment variable to take value from
                                 env-var
                                 ;; if true, then option does not require a value,
                                 ;; but becomes True if specified on the command-line
                                 flag
                                 ;; when not specified, then will be created from the
                                 ;; first character of the `name'
                                 ;; pass :short nil, to disable short name for the argument
                                 (short nil short-given-p)
                                 (default nil default-given-p))
  "Returns a single fields description.
   Name argument is a symbol.
   Function returns a list."

  ;; Type will be choosen from default if it was given,
  ;; or will be a flag or stropt otherwise
  ;; all supported types are described in clon's documentation
  ;; https://www.lrde.epita.fr/%7Edidier/software/lisp/clon/user/Built_002dIn-Valued-Options.html#Built_002dIn-Valued-Options
  (let ((result (list (cond (flag
                             'flag)
                            ((and default-given-p
                                  (typep (get-value-if-symbol default) 'integer))
                             'lispobj)
                            (t
                             'stropt))
                      :long-name (make-long-name name)
                      :env-var env-var
                      :description documentation)))
    (unless (and short-given-p
                 (null short))
      (setf result
            (append result
                    (list :short-name
                          (or short
                              (make-short-name name))))))
    
    (when default-given-p
      (setf result
            (append result
                    (list :default-value default))))

    result))


(defun add-help-fields (args)
  (flet ((is-already-exists (arg-name)
           (assoc arg-name (remove-if-not #'listp args)
                  :test #'string-equal
                  :key (lambda (item)
                         (symbol-name item)))))
    
    (unless (is-already-exists "help")
      (push (list 'help "Show help on this program." :flag t)
            args))

    (when (and (is-has-subcommand args)
               (not (is-already-exists "help-commands")))
      (push (list 'help-commands "Show a list of all supported commands." :flag t :short nil)
            args))

    (values args)))


(defun map-fields (function defmain-args)
  "Maps given function to all given args. Args should be in the format
   of defmain arguments.

   Returns a list of results from each function call."

  (loop for arg in (add-help-fields defmain-args)
        ;; All arguments before any &something key are considered
        ;; as fields descriptions
        when (and (symbolp arg)
                  (char= (elt (symbol-name arg)
                              0)
                         #\&))
          do (return-from map-fields
               (remove-if #'null
                          results))
        collect (etypecase arg
                  (symbol (funcall function arg))
                  (list (apply function arg)))
          into results
        finally (return (remove-if #'null
                                   results))))


(defun make-synopsis-fields (defmain-args)
  "Returns fields description for net.didierverna.clon:defsynopsis."
  (flet ((make-field (&rest args)
           (when (> (length args)
                    1)
             (apply 'make-field-description args))))
    (map-fields #'make-field defmain-args)))


(defun get-command-name (symbol)
  (let* ((package (symbol-package symbol))
         (package-name (string-downcase (package-name package)))
         (roswell-prefix "ros.script."))
    (cond ((starts-with package-name
                        roswell-prefix)
           (elt (split package-name ".")
                2))
          ;; If package wasn't generated by Roswell,
          ;; just use symbol's name
          (t (string-downcase (symbol-name symbol))))))


(defun make-binding (name &rest args)
  (declare (ignorable args))

  ;; if there is no args, then this is a positional argument,
  ;; not a --flag or --option
  ;; In this case, we dont generate a binding
  (when (not (null args))
    `(,name (getopt :long-name ,(string-downcase (symbol-name name))))))


(defun get-positional-args (defmain-args)
  (flet ((get-name (name &rest rest)
           (when (null rest)
             name)))
    (map-fields #'get-name defmain-args)))


(defun make-bindings (defmain-args)
  "Returns a list of forms for \"let\" form.
   Variable args contains a list of arguments given to defmain, like:

   \(\(debug :documentation \"Show traceback instead of short message.\"\)
    \(log   :documentation \"Filename to write log to.\"\)
    &rest repository\)

   For this input, output will be a list like:

   \(\(debug \(net.didierverna.clon:getopt :long-name \"debug\"\)\)
     \(log \(net.didierverna.clon:getopt :long-name \"log\"\)\)\)
"
  (map-fields #'make-binding defmain-args))


(defun make-positional-bindings (defmain-args)
  "Returns a list of forms for \"let\" form.
   It is like make-bindings, but only returns bindings for positional arguments.
   The should be separate because applied after the --help option was checked.
"
  (let ((bindings nil)
        (rest-arg (get-rest-arg defmain-args))
        (positional-args (get-positional-args defmain-args)))

    (when rest-arg
      (push `(,rest-arg %rest-arguments)
            bindings))
    
    (loop for arg in (reverse positional-args)
          do (push `(,arg (%pop-argument ',arg))
                   bindings))
    
    bindings))


(defun %print-commands-help (main-symbol &key (stream *standard-output*))
  "Outputs to stdout a help about command line utility."
  (format stream "These commands are supported:~2%")
  
  (let* ((commands (get main-symbol :subcommands))
         (commands (mapcar (lambda (c)
                             (cons (string-downcase c)
                                   (get-short-description c)))
                           commands))
         (commands (sort commands #'string< :key #'first)))
    
    
    (dolist (subcommand commands)
      (format stream " * ~A - ~A~%"
              (car subcommand)
              (cdr subcommand))))

  (format stream "~%"))


(defun %call-command (parent parent-arguments command-and-args)
  (let* ((command-name (first command-and-args))
         (args (rest command-and-args))
         ;; Search command's symbol by name
         (command (find command-name
                        (get parent :subcommands)
                        :test #'string-equal)))
    (unless command
      (if command-name
          (format *error-output* "Command \"~A\" was not found.~%"
                  (string-downcase command-name))
          (format *error-output* "Please, specify a command.~%"))
      (format *error-output* "~2&Here is a list of all supported commands:~2%")
      
      (%print-commands-help parent :stream *error-output*)
      (uiop:quit 1))
    
    (apply command
           (append parent-arguments
                   args))))


(defun extract-parent-args (args)
  "Searches in the list of macro arguments a sequence like:

   &parent-args (foo bar)

   and returns (foo bar)."
  (remove 'help
          (second (member '&parent-args args))))


(defclass cool-synopsis (net.didierverna.clon::synopsis)
  ((command :initarg :command
            :documentation "A symbol of a function created by defmain macro. Used to extract information about the name of the current command and a name of subcommands."
            :reader get-command)))


(defmethod net.didierverna.clon::help-spec ((synopsis cool-synopsis) &key program)
  (let ((command-name (get-command synopsis)))
    ;; If is synopsis is bound to a subcommand, then we'll
    ;; add it's name to the program name
    (call-next-method synopsis :program (if command-name
                                            (format nil "~A ~A"
                                                    program
                                                    (string-downcase command-name))
                                            program))))


(defun get-short-description (command-symbol)
  (let ((doc (documentation command-symbol 'function)))
    (when doc
      (first (split doc #\Newline)))))


(defun %is-need-to-catch-errors (args)
  "Checks if option :catch-errors t was given to the defmacro.
   If not given, then it is True by default."
  (let ((found (member :catch-errors args)))
    (if found
        (second found)
        t)))


(defmacro defmain (name (&rest args) &body body)
  (let* ((command-name (get-command-name name))
         (synopsis-args (make-synopsis-args args))
         (synopsis-fields (make-synopsis-fields args))
         (docstring (when (typep (first body)
                                 'string)
                      (prog1 (first body)
                        (setf body (rest body)))))
         (synopsis-description
           (when docstring
             `((text :contents ,docstring))))

         (bindings (make-bindings args))
         (positional-bindings (make-positional-bindings args))
         (local-functions
             (when positional-bindings
               ;; We only need this function when there is one or
               ;; more positional bindings exist.
               (list '(%pop-argument (name)
                       "This local function is used to pop positional arguments from the command line."
                       (unless %rest-arguments
                         (check-type name symbol)
                         (error 'argument-is-required-error
                                :name name))
                       (pop %rest-arguments)))))
         ;; Here we'll store only parent variable names
         (argument-names (remove 'help
                                 (mapcar #'first
                                         bindings)))
         (has-subcommand-p (is-has-subcommand args))
         (handle-conditions-p (%is-need-to-catch-errors args))
         (parent-arguments (extract-parent-args args))
         (help-opt-provided-p (remove-if-not
                               (lambda (binding)
                                 (let ((command (first binding)))
                                   ;; We are searching an option help
                                   (and (string-equal (symbol-name command)
                                                      "help")
                                        ;; And it shouldn't be from the defmain package
                                        (not (eql command
                                                  'help)))))
                               bindings))
         (subcommand-was-called (gensym)))

    `(progn
       (defun ,name (,@parent-arguments &rest argv)
         (declare (ignorable ,@parent-arguments))
         
         (let ((synopsis (defsynopsis (,@synopsis-args :make-default nil)
                           ,@synopsis-description
                           ,@synopsis-fields))
               (argv (or argv
                         ;; We need this to support usage of defmain
                         ;; in programs, built with plain asdf:make
                         ;; instead of roswell.
                         (uiop:command-line-arguments))))
           (change-class synopsis
                         'cool-synopsis
                         :command ',(unless has-subcommand-p
                                      name))
           (make-context
            :cmdline (cons ,command-name argv)
            :synopsis synopsis))

         (let ((%rest-arguments (remainder)))
           (declare (ignorable %rest-arguments))
           
           (flet (,@local-functions)
             (let (,@bindings
                   ,@(when has-subcommand-p
                       `((,subcommand-was-called nil))))
               ;; Sometimes user may want to redefine a help option
               ;; in this case we shouldn't decide how to print help for him.
               ,(unless help-opt-provided-p
                  `(when help
                     (help)
                     (uiop:quit 1)))

               ,@(when has-subcommand-p
                   `((when help-commands
                       (%print-commands-help ',name)
                       (uiop:quit 1))))

               (handler-bind (,@(when handle-conditions-p 
                                  '((#+ccl ccl:interrupt-signal-condition
                                     #+sbcl sb-sys:interactive-interrupt
                                     #+clisp system::simple-interrupt-condition
                                     #+ecl ext:interactive-interrupt
                                     #+allegro excl:interrupt-signal
                                     (lambda (c)
                                       (declare (ignorable c))
                                       (uiop:quit 0)))
                                    (argument-is-required-error
                                     (lambda (c)
                                       (format t "~A~%" c)
                                       (uiop:quit 1)))
                                    (error (lambda (condition)
                                             (uiop:print-condition-backtrace condition :stream *error-output*)
                                             (uiop:quit 1))))))
                 ;; Positional arguments are processed here because this should happen
                 ;; after the --help option was processed. Because otherwise there will
                 ;; be "argument is required" error when you only give --help option.
                 (let (,@positional-bindings)
                   ;; Functions (print-commands-help) and (subcommand)
                   ;; should only be available if currently defined
                   ;; procedure has subcommands
                   (flet (,@(when has-subcommand-p
                              `((print-commands-help ()
                                                     (%print-commands-help ',name))
                                (subcommand ()
                                            (%call-command ',name
                                                           (list ,@argument-names)
                                                           (remainder))
                                            (setf ,subcommand-was-called t))
                                (get-subcommand-name ()
                                                     (first (remainder))))))

                     ;; If cl-fad package is used, then we need to reset
                     ;; the logical pathname, it remembers during load.
                     ;; 
                     ;; This is needed because this pathname can be compiled into the
                     ;; binary and absent on the machine where this binary was executed.
                     ,@(when (find-package :cl-fad)
                         `((setf (logical-pathname-translations "TEMPORARY-FILES")
                                 `(("*.*.*" ,(uiop:symbol-call :cl-fad 'get-default-temporary-directory))))))
                     ;; Also, we need to reset TEMP path inside UIOP:
                     (uiop:setup-temporary-directory)
                    
                     ,@body

                     ;; If user didn't called (subcommand) explicitly,
                     ;; we'll do this call implicitly after his/her body.
                     ,(when has-subcommand-p
                        `(unless ,subcommand-was-called
                           (subcommand))))))))))

       ;; Now we'll store all main function's argument names into it's
       ;; property list, to reuse them in subcommands
       (setf (get ',name :arguments)
             ',argument-names
             (documentation ',name 'function)
             ,docstring))))


(defmacro defcommand ((parent name) (&rest args) &body body)
  (declare (ignorable parent))
  (let ((parent-args (get parent :arguments)))
    `(progn
       (defmain ,name (,@args &parent-args ,parent-args :catch-errors nil)
         ,@body)

       ;; Now we'll register subcommand in it's parent
       (pushnew ',name
                (get ',parent :subcommands)))))


(defun print-help ()
  "Outputs to stdout a help about command line utility."
  (help))
