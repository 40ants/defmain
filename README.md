<a id='x-28DEFMAIN-2FDEFMAIN-3A-40INDEX-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

# DEFMAIN - intuitive command line options parser for Common Lisp

## Table of Contents

- [1 defmain ASDF System Details][f561]
- [2 Reasoning][9447]
- [3 Installation][b512]
- [4 Roadmap][d6d7]

###### \[in package DEFMAIN/DEFMAIN with nicknames DEFMAIN\]
<a id='x-28-23A-28-287-29-20BASE-CHAR-20-2E-20-22defmain-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29'></a>

## 1 defmain ASDF System Details

- Version: 0.10.0
- Description: A wrapper around net.didierverna.clon which makes command line arguments parsing easier.
- Licence: BSD
- Author: Alexander Artemenko
- Homepage: [https://40ants.com/defmain](https://40ants.com/defmain)
- Bug tracker: [https://github.com/40ants/defmain/issues](https://github.com/40ants/defmain/issues)
- Source control: [GIT](https://github.com/40ants/defmain)

[![](https://github-actions.40ants.com/40ants/defmain/matrix.svg)](https://github.com/40ants/defmain/actions)

<a id='x-28DEFMAIN-2FDEFMAIN-3A-40REASONING-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 2 Reasoning

Library [`net.didierverna.clon`](https://github.com/didierverna/clon)
very powerful, but too complicated to use in simple cases. This library
provides a wrapper which will suite your needs in 80% cases.

Compare this code, which uses [`DEFMAIN`][f561] macro:

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


<a id='x-28DEFMAIN-2FDEFMAIN-3A-40INSTALLATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 3 Installation

This system is available as part of the https://ultralisp.org distribution. Follow instruction
on the site to setup the distribution, and then install [`DEFMAIN`][f561] system using Quicklisp client:

    (ql:quickload :defmain)


<a id='x-28DEFMAIN-2FDEFMAIN-3A-40ROADMAP-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 4 Roadmap

- Make better support for integer arguments.

- Support more types of arguments, like filepathes and enums.

- Raise error when two short options are identical during
  macro-expansion, not during runtime. Right now the `clon`
  checks this during runtime:

```
Unhandled SIMPLE-ERROR in thread #<SB-THREAD:THREAD "main thread"
RUNNING {10005285B3}>:

Options #<LISPOBJ {1002705593}> and #<STROPT {1002705C03}>:
indentical short name "s".

Backtrace for: #<SB-THREAD:THREAD "main thread" RUNNING
{10005285B3}>
0: (`SB-DEBUG::DEBUGGER-DISABLED-HOOK` #<SIMPLE-ERROR "Options ~A and
~A: indentical short name ~S." {100277D8F3}> #<unused argument>
:QUIT T)
1: (`SB-DEBUG::RUN-HOOK` SB-EXT:*INVOKE-DEBUGGER-HOOK* #<SIMPLE-ERROR
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


  [9447]: #x-28DEFMAIN-2FDEFMAIN-3A-40REASONING-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Reasoning"
  [b512]: #x-28DEFMAIN-2FDEFMAIN-3A-40INSTALLATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Installation"
  [d6d7]: #x-28DEFMAIN-2FDEFMAIN-3A-40ROADMAP-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Roadmap"
  [f561]: #x-28-23A-28-287-29-20BASE-CHAR-20-2E-20-22defmain-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29 "(#A((7) BASE-CHAR . \"defmain\") ASDF/SYSTEM:SYSTEM)"

* * *
###### \[generated by [40ANTS-DOC](https://40ants.com/doc)\]
