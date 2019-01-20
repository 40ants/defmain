=================
 defmain
=================

.. insert-your badges like that:

.. image:: https://travis-ci.org/40ants/defmain.svg?branch=master
    :target: https://travis-ci.org/40ants/defmain

.. Everything starting from this commit will be inserted into the
   index page of the HTML documentation.
.. include-from

Give some introduction.
This system contains a little helper for easy command line arguments
parsing. It is based on great, poverfull, but complicated
`net.didierverna.clon <https://github.com/didierverna/clon>`_ and
assumes you are writing script using Roswell.

Reasoning
=========

Library `net.didierverna.clon <https://github.com/didierverna/clon>`_
very powerful, but too complicated to use in simple cases. This library
provides a wrapper which will suite in 80% cases.

Compare this code, which uses ``defmain``:

.. code-block:: common-lisp

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

With code providing same functionality, but using raw
net.didierverna.clon:

.. code-block:: common-lisp

   (net.didierverna.clon:defsynopsis (:postfix "REPOSITORY")
     (text :contents "This utility builds a report about all non-merged commits for any github repository. Just give some repository name like \"antirez/redis\" as an argument and pipe stdout to some file.
   ")
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

Installation
============

This system is available as part of the http://Ultralisp.org
distribution.

Roadmap
=======

* Make better support for integer arguments.
* Support more types of arguments, like filepathes and enums.
* Raise error when two short options are identical during
  macro-expansion, not during runtime. Right now the `clon`
  checks this during runtime::

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
    

.. Everything after this comment will be omitted from HTML docs.
.. include-to

