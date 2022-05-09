<a id="x-28DEFMAIN-3A-40README-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# DEFMAIN - intuitive command line options parser for Common Lisp

<a id="defmain-asdf-system-details"></a>

## DEFMAIN ASDF System Details

* Version: 0.12.1

* Description: A wrapper around net.didierverna.clon which makes command line arguments parsing easier.

* Licence: `BSD`

* Author: Alexander Artemenko

* Homepage: [https://40ants.com/defmain][a9ad]

* Bug tracker: [https://github.com/40ants/defmain/issues][defe]

* Source control: [GIT][26c5]


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

<a id="x-28DEFMAIN-3A-3A-40REASONING-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Reasoning

Library [`net.didierverna.clon`][da87]
very powerful, but too complicated to use in simple cases. This library
provides a wrapper which will suite your needs in 80% cases.

Compare this code, which uses [`defmain`][4130] macro:

```
(defmain (main) ((debug "Show traceback instead of short message."
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
<a id="x-28DEFMAIN-3A-3A-40INSTALLATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Installation

This system is available as part of the https://ultralisp.org distribution. Follow instruction
on the site to setup the distribution, and then install [`defmain`][dc1a] system using Quicklisp client:

```text
(ql:quickload :defmain)
```
<a id="x-28DEFMAIN-3A-3A-40USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Usage

The main entry point for defining the main function for your program is the [`defmain`][4130] macro:

<a id="x-28DEFMAIN-3ADEFMAIN-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

### [macro](fe4f) `defmain:defmain` (name &key program-name) (&rest args) &body body

This macro let you to define a main function for a command-line program.

Usually the `NAME` argument will be just `MAIN`. This name will be bound
to a function which will process arguments and execute the `BODY`.

`ARGS` should contain an arguments definition. Each definition is a list of the form:

```text
(NAME DESCRIPTION &KEY FLAG ENV-VAR SHORT DEFAULT)
```
Argument's `NAME` should be a symbol. It names a variable which will be bound during
the `BODY` execution. Also, this name is lowercased and used to form a `--long`
command line argument.

The lowercased first letter of the `NAME` is used as a short version of the argument,
like `-l`. But sometimes you might encounter duplication errors when having
a few arguments starting from the same letter. In this case provide `SHORT` option,
to override the letter, used for the short option.

For example, here we have a conflict:

```
(defmain (main) ((version "Print program version and exit")
                 (verbose "Provide more detail on the output"))
   ...)
```
But we can tell `defmain` ([`1`][4130] [`2`][dc1a]) to use `-V` option for verbose, instead of `-v`

```
(defmain (main) ((version "Print program version and exit")
                 (verbose "Provide more detail on the output" :short "V"))
   ...)
```
Also, we can pass `NIL`, to turn off short version for `VERBOSE` argument:

```
(defmain (main) ((version "Print program version and exit")
                 (verbose "Provide more detail on the output" :short NIL))
   ...)
```
If some of your options are boolean, then give it a `:FLAG t` option,
and a variable will become `T` if user provided this flag on the command-line.

Also, you might want to specify a `DEFAULT` value for the argument or provide
an environment variable name using `ENV-VAR`. The value will be take from the
environment variable unless it was provided by the user on the command-line.

Arguments list of [`defmain`][4130] macro might end with `&REST SOME-VAR`. In this case,
all unprocessed command line arguments will be collected into the `SOME-VAR` list.

By default program name, shown in the `--help`, will be the same as the name
of the function or taken as a third part of the `ROS.SCRIPT.THIRD-PART` package
name, if you are using Roswell. However, you can override it providing the
`PROGRAM-NAME` argument.

 

<a id="x-28DEFMAIN-3A-3A-40SUBCOMMANDS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Subcommands

Also, you might want to build a more complex command-line interface with subcommands.

In this case, you need to use [`defmain`][4130] macro to define the main entry-point, and then
to define additional subcommands using [`defcommand`][4b6c] macro:

<a id="x-28DEFMAIN-3ADEFCOMMAND-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

#### [macro](1264) `defmain:defcommand` (parent name) (&rest args) &body body

This macro is similar to [`defmain`][4130] macro in terms of arguments and body processing.

The only difference is that instead of the single name you have to provide a
list of two names:

* First element should be the name of the parent function.
  It can be either a main entry-point or other subcommand.

* Second element is a symbol to name the subcommand.

Here is an example with of a program with two subcommands.
Pay attention to the `MAIN` function's argument list.
It ends with a special symbol `&SUBCOMMAND`. It should be
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

To run subcommand, execute [`subcommand`][98fc] function:

```
(defmain (main) ((verbose "More detail in the output"))
  (format t "Before subcommand.~%")
  (defmain:subcommand)
  (format t "After subcommand.~%"))
```
  

<a id="x-28DEFMAIN-3A-3A-40HELPERS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Helpers

When writing more complex logic, these helpers could be useful:

<a id="x-28DEFMAIN-3APRINT-HELP-20FUNCTION-29"></a>

#### [function](3685) `defmain:print-help`

Outputs to stdout a help about command line utility.

<a id="x-28DEFMAIN-3APRINT-COMMANDS-HELP-20FUNCTION-29"></a>

#### [function](97d8) `defmain:print-commands-help`

Outputs information about supported subcommands.

It should be called from the function defined with [`defmain`][4130] macro.

<a id="x-28DEFMAIN-3AGET-SUBCOMMAND-NAME-20FUNCTION-29"></a>

#### [function](78c1) `defmain:get-subcommand-name`

Returns a string with current subcommand's name.

It should be called from the function defined with [`defmain`][4130] macro.

<a id="x-28DEFMAIN-3ASUBCOMMAND-20FUNCTION-29"></a>

#### [function](48dc) `defmain:subcommand`

Executes the current subcommand. It is called automatically at the end of the
main body unless you call it manually.

It can be called from the function defined with [`defmain`][4130] macro.

<a id="x-28DEFMAIN-3A-3A-40ROADMAP-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Roadmap

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

[a9ad]: https://40ants.com/defmain
[dc1a]: https://40ants.com/defmain/#x-28-23A-28-287-29-20BASE-CHAR-20-2E-20-22defmain-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29
[4b6c]: https://40ants.com/defmain/#x-28DEFMAIN-3ADEFCOMMAND-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29
[4130]: https://40ants.com/defmain/#x-28DEFMAIN-3ADEFMAIN-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29
[98fc]: https://40ants.com/defmain/#x-28DEFMAIN-3ASUBCOMMAND-20FUNCTION-29
[26c5]: https://github.com/40ants/defmain
[97d8]: https://github.com/40ants/defmain/blob/4eceea70b9484820c3ba81121d69ab03dd5a9c80/src/defmain.lisp#L482
[78c1]: https://github.com/40ants/defmain/blob/4eceea70b9484820c3ba81121d69ab03dd5a9c80/src/defmain.lisp#L489
[48dc]: https://github.com/40ants/defmain/blob/4eceea70b9484820c3ba81121d69ab03dd5a9c80/src/defmain.lisp#L496
[fe4f]: https://github.com/40ants/defmain/blob/4eceea70b9484820c3ba81121d69ab03dd5a9c80/src/defmain.lisp#L589
[1264]: https://github.com/40ants/defmain/blob/4eceea70b9484820c3ba81121d69ab03dd5a9c80/src/defmain.lisp#L828
[3685]: https://github.com/40ants/defmain/blob/4eceea70b9484820c3ba81121d69ab03dd5a9c80/src/defmain.lisp#L888
[defe]: https://github.com/40ants/defmain/issues
[da87]: https://github.com/didierverna/clon

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
