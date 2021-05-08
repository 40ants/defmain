(uiop:define-package defmain/defmain
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


(defsection @index (:title "DEFMAIN - intuitive command line options parser for Common Lisp"
                    :ignore-words ("VERBOSE"
                                   "MAIN"
                                   "SOME-VAR"))
  (defmain system)
  "
<table>
<tr>
  <th>Tests:</th>
  <td><a style=\"border-bottom: none\" href=\"https://github.com/40ants/defmain/actions\"><img src=\"https://github-actions.40ants.com/40ants/defmain/matrix.svg?only=ci.run-tests\"></a></td>
</tr>
<tr>
  <th>Linter:</th>
  <td><a style=\"border-bottom: none\" href=\"https://github.com/40ants/defmain/actions\"><img src=\"https://github-actions.40ants.com/40ants/defmain/matrix.svg?only=ci.linter\"></a></td>
</tr>
</table>
"
  (@reasoning section)
  (@installation section)
  (@usage section)
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


(defsection @usage (:title "Usage")
  "The main entry point for defining the main function for your program is the DEFMAIN macro:"

  (defmain macro)
  (@subcommands section)
  (@helpers section))


(defsection @subcommands (:title "Subcommands")
  "
Also, you might want to build a more complex command-line interface with subcommands.

In this case, you need to use DEFMAIN macro to define the main entry-point, and then
to define additional subcommands using DEFCOMMAND macro:
"

  (defcommand macro))


(defsection @helpers (:title "Helpers")
  "
When writing more complex logic, these helpers could be useful:
"

  (print-help function)
  (print-commands-help function)
  (get-subcommand-name function)
  (subcommand function))


(defsection @roadmap (:title "Roadmap"
                      :ignore-words ("RUNNING"
                                     "*INVOKE-DEBUGGER-HOOK*"))
  """
* Make better support for integer arguments.
* Support more types of arguments, like filepathes and enums.
* Raise error when two short options are identical during
  macro-expansion, not during runtime. Right now the `clon`
  checks this during runtime:
  
```
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
```
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
                                 ;; first character of the NAME
                                 ;; pass :SHORT nil, to disable short name for the argument
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

  (loop with skip-n-args = 0
        for arg in (add-help-fields defmain-args)
        ;; All arguments before any &something key are considered
        ;; as fields descriptions
        when (and (symbolp arg)
                  (char= (elt (symbol-name arg)
                              0)
                         #\&))
          do (return-from map-fields
               (remove-if #'null
                          results))
        when (keywordp arg)
          do (setf skip-n-args 2)
        when (zerop skip-n-args)
          collect (etypecase arg
                    (symbol (funcall function arg))
                    (list (apply function arg)))
            into results
        unless (zerop skip-n-args)
          do (decf skip-n-args)
        finally (return (remove-if #'null
                                   results))))


(defun make-synopsis-fields (defmain-args)
  "Returns fields description for net.didierverna.clon:defsynopsis."
  (flet ((make-field (&rest args)
           (when (> (length args)
                    1)
             (apply 'make-field-description args))))
    (map-fields #'make-field defmain-args)))


(defun get-program-name (symbol)
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


(defun print-commands-help ()
  "Outputs information about supported subcommands.

   It should be called from the function defined with DEFMAIN macro."
  (error "Function PRINT-COMMANDS-HELP is only available inside DEFMAIN macro."))


(defun get-subcommand-name ()
  "Returns a string with current subcommand's name.

   It should be called from the function defined with DEFMAIN macro."
  (error "Function GET-SUBCOMMAND-NAME is only available inside DEFMAIN macro."))


(defun subcommand ()
  "Executes the current subcommand. It is called automatically at the end of the
   main body unless you call it manually.

   It can be called from the function defined with DEFMAIN macro."
  (error "Function SUBCOMMAND is only available inside DEFMAIN macro."))


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
                   (list 'subcommand-args)
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


(defmacro defmain ((name &key program-name)
                   (&rest args) &body body)
  """
This macro let you to define a main function for a command-line program.

Usually the NAME argument will be just MAIN. This name will be bound
to a function which will process arguments and execute the BODY.

ARGS should contain an arguments definition. Each definition is a list of the form:

    (NAME DESCRIPTION &KEY FLAG ENV-VAR SHORT DEFAULT)

Argument's NAME should be a symbol. It names a variable which will be bound during
the BODY execution. Also, this name is lowercased and used to form a `--long`
command line argument.

The lowercased first letter of the NAME is used as a short version of the argument,
like `-l`. But sometimes you might encounter duplication errors when having
a few arguments starting from the same letter. In this case provide SHORT option,
to override the letter, used for the short option.

For example, here we have a conflict:

```
(defmain (main) ((version "Print program version and exit")
                 (verbose "Provide more detail on the output"))
   ...)
```

But we can tell DEFMAIN to use `-V` option for verbose, instead of `-v`

```
(defmain (main) ((version "Print program version and exit")
                 (verbose "Provide more detail on the output" :short "V"))
   ...)
```

Also, we can pass NIL, to turn off short version for VERBOSE argument:

```
(defmain (main) ((version "Print program version and exit")
                 (verbose "Provide more detail on the output" :short NIL))
   ...)
```

If some of your options are boolean, then give it a `:FLAG t` option,
and a variable will become `T` if user provided this flag on the command-line.

Also, you might want to specify a DEFAULT value for the argument or provide
an environment variable name using ENV-VAR. The value will be take from the
environment variable unless it was provided by the user on the command-line.

Arguments list of DEFMAIN macro might end with `&REST SOME-VAR`. In this case,
all unprocessed command line arguments will be collected into the SOME-VAR list.

By default program name, shown in the `--help`, will be the same as the name
of the function or taken as a third part of the ROS.SCRIPT.THIRD-PART package
name, if you are using Roswell. However, you can override it providing the
PROGRAM-NAME argument.

 """
  (let* ((program-name (or program-name
                           (get-program-name name)))
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

    `(eval-when
         ;; Without this, arguments will not be stored in the
         ;; property list, when program was compiled using ASDF.
         ;; Compilation with Roswell worked with PROGN instead of EVAL-WHEN :(
         (:compile-toplevel :load-toplevel :execute)
       (defun ,name (,@parent-arguments &rest argv)
         (declare (ignorable ,@parent-arguments))
         
         (let ((synopsis (defsynopsis (,@synopsis-args :make-default nil)
                           ,@synopsis-description
                           ,@synopsis-fields))
               (argv
                 (cond
                   ;; We need this rule to distinguish the case
                   ;; when subcommand receives no arguments.
                   ;; In such case we should not try to call
                   ;; UIOP:COMMAND-LINE-ARGUMENTS
                   ((and argv
                         (eql (first argv)
                              'subcommand-args))
                    (cdr argv))
                   (argv
                    argv)
                   (t
                    ;; We need this to support usage of defmain
                    ;; in programs, built with plain asdf:make
                    ;; instead of roswell.
                    (uiop:command-line-arguments)))))
           (change-class synopsis
                         'cool-synopsis
                         :command ',(unless has-subcommand-p
                                      name))
           (make-context
            :cmdline (cons ,program-name argv)
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
                     (uiop:quit 0)))

               ,@(when has-subcommand-p
                   `((when help-commands
                       (%print-commands-help ',name)
                       (uiop:quit 0))))

               (handler-bind (,@(when handle-conditions-p 
                                  '(#+(or ccl sbcl clisp ecl allegro)
                                    (#+ccl ccl:interrupt-signal-condition
                                     #+sbcl sb-sys:interactive-interrupt
                                     #+clisp system::simple-interrupt-condition
                                     #+ecl ext:interactive-interrupt
                                     #+allegro excl:interrupt-signal
                                     (lambda (c)
                                       (declare (ignore c))
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
                     ,@(when has-subcommand-p
                         '((declare (ignorable
                                     (function get-subcommand-name)
                                     (function print-commands-help)))))
                     

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
             (get ',name :program-name)
             ',program-name
             (documentation ',name 'function)
             ,docstring))))


(defmacro defcommand ((parent name) (&rest args) &body body)
  """
This macro is similar to DEFMAIN macro in terms of arguments and body processing.

The only difference is that instead of the single name you have to provide a
list of two names:

- First element should be the name of the parent function.
  It can be either a main entry-point or other subcommand.
- Second element is a symbol to name the subcommand.

Here is an example with of a program with two subcommands.
Pay attention to the `MAIN` function's argument list.
It ends with a special symbol &SUBCOMMAND. It should be
provided to let macro know there will be some subcommands
defined later. 

```
(defmain (main) ((verbose "More detail in the output")
                 &subcommand)
   ...)

(defcommand (main upload) ((upstream "Repository name")
                           (force "Rewrite changes in case of conflict"
                                  :flag t))
   ...)

(defcommand (main sync) ()
  "Yet another subcommand."
   ...)
```

All arguments, specified for the `MAIN` function also bound for all it's subcommands.
On command-line these arguments should preceed the subcommand's name

By default, main command run's specified subcommand and exits, but you can use
it as a decorator, to execute some common code before and after as subcommand.

To run subcommand, execute SUBCOMMAND function:

```
(defmain (main) ((verbose "More detail in the output"))
  (format t "Before subcommand.~%")
  (defmain:subcommand)
  (format t "After subcommand.~%"))
```

  """
  (let ((parent-args (get parent :arguments))
        (parent-program-name (get parent :program-name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defmain (,name :program-name ,parent-program-name)
           (,@args &parent-args ,parent-args :catch-errors nil)
         ,@body)

       ;; Now we'll register subcommand in it's parent
       (pushnew ',name
                (get ',parent :subcommands)))))


(defun print-help ()
  "Outputs to stdout a help about command line utility."
  (help))
