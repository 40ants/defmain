<a id='x-28DEFMAIN-2FDEFMAIN-3A-40INDEX-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

# DEFMAIN - intuitive command line options parser for Common Lisp

## Table of Contents

- [1 defmain ASDF System Details][f561]
- [2 Reasoning][9447]
- [3 Installation][b512]
- [4 Usage][e173]
    - [4.1 Subcommands][1d0e]
- [5 Roadmap][d6d7]

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

<table>
<tr>
  <th>Tests:</th>
  <td><a style="border-bottom: none" href="https://github.com/40ants/defmain/actions"><img src="https://github-actions.40ants.com/40ants/defmain/matrix.svg?only=ci.run-tests"></a></td>
</tr>
<tr>
  <th>Linter:</th>
  <td><a style="border-bottom: none" href="https://github.com/40ants/defmain/actions"><img src="https://github-actions.40ants.com/40ants/defmain/matrix.svg?only=ci.linter"></a></td>
</tr>
</table>

<a id='x-28DEFMAIN-2FDEFMAIN-3A-40REASONING-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 2 Reasoning

Library [`net.didierverna.clon`](https://github.com/didierverna/clon)
very powerful, but too complicated to use in simple cases. This library
provides a wrapper which will suite your needs in 80% cases.

Compare this code, which uses [`DEFMAIN`][5924] macro:

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
on the site to setup the distribution, and then install [`DEFMAIN`][5924] system using Quicklisp client:

    (ql:quickload :defmain)


<a id='x-28DEFMAIN-2FDEFMAIN-3A-40USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 4 Usage

The main entry point for defining the main function for your program is the [`DEFMAIN`][5924] macro:

<a id='x-28DEFMAIN-2FDEFMAIN-3ADEFMAIN-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29'></a>

- [macro] **DEFMAIN** *NAME (&REST ARGS) &BODY BODY*

    This macro let you to define a main function for a command-line program.
    
    Usually the `NAME` argument will be just MAIN. This name will be bound
    to a function which will process arguments and execute the `BODY`.
    
    `ARGS` should contain an arguments definition. Each definition is a list of the form:
    
        (NAME DESCRIPTION &KEY FLAG ENV-VAR SHORT DEFAULT)
    
    Argument's `NAME` should be a symbol. It names a variable which will be bound during
    the `BODY` execution. Also, this name is lowercased and used to form a `--long`
    command line argument.
    
    The lowercased first letter of the `NAME` is used as a short version of the argument,
    like `-l`. But sometimes you might encounter duplication errors when having
    a few arguments starting from the same letter. In this case provide `SHORT` option,
    to override the letter, used for the short option.
    
    For example, here we have a conflict:
    
    ```
    (defmain main ((version "Print program version and exit")
                   (verbose "Provide more detail on the output"))
       ...)
    ```
    
    But we can tell [`DEFMAIN`][5924] to use `-V` option for verbose, instead of `-v`
    
    ```
    (defmain main ((version "Print program version and exit")
                   (verbose "Provide more detail on the output" :short "V"))
       ...)
    ```
    
    Also, we can pass `NIL`, to turn off short version for VERBOSE argument:
    
    ```
    (defmain main ((version "Print program version and exit")
                   (verbose "Provide more detail on the output" :short NIL))
       ...)
    ```
    
    If some of your options are boolean, then give it a `:FLAG t` option,
    and a variable will become `T` if user provided this flag on the command-line.
    
    Also, you might want to specify a `DEFAULT` value for the argument or provide
    an environment variable name using `ENV-VAR`. The value will be take from the
    environment variable unless it was provided by the user on the command-line.
    
    Arguments list of [`DEFMAIN`][5924] macro might end with `&REST SOME-VAR`. In this case,
    all unprocessed command line arguments will be collected into the SOME-VAR list.
    
     

<a id='x-28DEFMAIN-2FDEFMAIN-3A-40SUBCOMMANDS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

### 4.1 Subcommands

Also, you might want to build a more complex command-line interface with subcommands.

In this case, you need to use [`DEFMAIN`][5924] macro to define the main entry-point, and then
to define additional subcommands using [`DEFCOMMAND`][5b01] macro:

<a id='x-28DEFMAIN-2FDEFMAIN-3ADEFCOMMAND-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29'></a>

- [macro] **DEFCOMMAND** *(PARENT NAME) (&REST ARGS) &BODY BODY*

    This macro is similar to [`DEFMAIN`][5924] macro in terms of arguments and body processing.
    
    The only difference is that instead of the symbolic name you have to provide a
    list of two names:
    
    - First element should be the name of the parent function.
      It can be either a main entry-point or other subcommand.
    
    - Second element is a symbol to name the subcommand.
    
    Here is an example with of a program with two subcommands:
    
    ```
    (defmain main ((verbose "More detail in the output"))
       ...)
    
    (defcommand (main push) ((upstream "Repository name")
                             (force "Rewrite changes in case of conflict"
                                    :flag t))
       ...)
    
    (defcommand (main update) ()
      "Yet another subcommand."
       ...)
    ```
    
      

<a id='x-28DEFMAIN-2FDEFMAIN-3A-40ROADMAP-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 5 Roadmap

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


  [1d0e]: #x-28DEFMAIN-2FDEFMAIN-3A-40SUBCOMMANDS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Subcommands"
  [5924]: #x-28DEFMAIN-2FDEFMAIN-3ADEFMAIN-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29 "(DEFMAIN/DEFMAIN:DEFMAIN (40ANTS-DOC/LOCATIVES:MACRO))"
  [5b01]: #x-28DEFMAIN-2FDEFMAIN-3ADEFCOMMAND-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29 "(DEFMAIN/DEFMAIN:DEFCOMMAND (40ANTS-DOC/LOCATIVES:MACRO))"
  [9447]: #x-28DEFMAIN-2FDEFMAIN-3A-40REASONING-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Reasoning"
  [b512]: #x-28DEFMAIN-2FDEFMAIN-3A-40INSTALLATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Installation"
  [d6d7]: #x-28DEFMAIN-2FDEFMAIN-3A-40ROADMAP-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Roadmap"
  [e173]: #x-28DEFMAIN-2FDEFMAIN-3A-40USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Usage"
  [f561]: #x-28-23A-28-287-29-20BASE-CHAR-20-2E-20-22defmain-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29 "(#A((7) BASE-CHAR . \"defmain\") ASDF/SYSTEM:SYSTEM)"

* * *
###### \[generated by [40ANTS-DOC](https://40ants.com/doc)\]
