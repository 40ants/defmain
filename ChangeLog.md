<a id="x-28DEFMAIN-2FCHANGELOG-3A-40CHANGELOG-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# ChangeLog

<a id="x-28DEFMAIN-2FCHANGELOG-3A-3A-7C0-2E13-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.13.0

* Moved to a new documentation rendering engine.

<a id="x-28DEFMAIN-2FCHANGELOG-3A-3A-7C0-2E12-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.12.1

* Added `(declare (ignorable %pop-argument))` to suppress a compiler `NOTE` under `SBCL`.

Thanks to Alessio Stalla!

<a id="x-28DEFMAIN-2FCHANGELOG-3A-3A-7C0-2E12-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.12.0

* Added LispWorks support.

* Program name now evaluated at runtime instead of a compile-time.

<a id="x-28DEFMAIN-2FCHANGELOG-3A-3A-7C0-2E11-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.11.0

<a id="backward-incompatible-change"></a>

### Backward incompatible change!

Now the name of a function given to the
[`defmain:defmain`][4130] macro, should be a list, because it
can carry optional keyword arguments, such as
`PROGRAM-NAME` argument.

<a id="other-changes"></a>

### Other changes

* Fixed subcommands for the case when a program was
  compiled using `ASDF:MAKE`.

* Added more examples and improved documentation!

<a id="x-28DEFMAIN-2FCHANGELOG-3A-3A-7C0-2E10-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.10.0

* Moved documentation from reStructured readme to `40ANTS-DOC`
  documentation builder.

* Added github actions for building docs, linting and testing.
  Using [`40ANTS-CI`][3f72] generator.

<a id="x-28DEFMAIN-2FCHANGELOG-3A-3A-7C0-2E9-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.9.1

* Fixed work under `ABCL` and probably `CMUCL`.

* Help commands now exit with 0 status code.

* Fixed annoying banner about missing `CC` env variable.

<a id="x-28DEFMAIN-2FCHANGELOG-3A-3A-7C0-2E9-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.9.0

* Added support for programs, built with `ASDF:MAKE`.

Previously, only [roswell][795a] was supported, but now
you can define your system as:

```lisp

(defsystem work-hours
  :class :package-inferred-system
  :build-operation "program-op"
  :build-pathname "work-hours"
  :entry-point "work-hours/main:main"
  :depends-on ("work-hours/main"))
```
And then call in the command line something like:

```bash
qlot exec ros run -L sbcl -e '(asdf:make :work-hours)'
```
<a id="x-28DEFMAIN-2FCHANGELOG-3A-3A-7C0-2E8-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.8.0

* Added support for positional arguments.

* Fixed issue of catching signals not inherited  `error` and
  handling them as errors by printing traceback and quitting.

<a id="x-28DEFMAIN-2FCHANGELOG-3A-3A-7C0-2E7-2E2-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.7.2

* Added `Ctrl-C` handling. However seems it does not work for `CCL` :(

<a id="x-28DEFMAIN-2FCHANGELOG-3A-3A-7C0-2E7-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.7.1

Error `Undeclared free variable DEFMAIN/DEFMAIN:HELP-COMMANDS` was
fixed for cases where there is no [`defmain:defcommand`][4b6c] calls beside [`defmain:defmain`][4130].

<a id="x-28DEFMAIN-2FCHANGELOG-3A-3A-7C0-2E7-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.7.0

Defmain macro now restores temporary pathnames inside of `cl-fad` and
`uiop` packages by setting `logical-pathname-translations` for
`"TEMPORARY-FILES"` and by calling
`(uiop:setup-temporary-directory)`.

This solves issues in programs which use `(uiop:run-program ...)` or
`(cl-fad:with-output-to-temporary-file ...)` for example.

<a id="x-28DEFMAIN-2FCHANGELOG-3A-3A-7C0-2E6-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.6.1

* Now function [`defmain:get-subcommand-name`][86bc] is available in the main function
if your specified `&SUBCOMMAND` as it's argument.

<a id="x-28DEFMAIN-2FCHANGELOG-3A-3A-7C0-2E6-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.6.0

* Added support for nested commands defined with [`defmain:defcommand`][4b6c] macro.

<a id="x-28DEFMAIN-2FCHANGELOG-3A-3A-7C0-2E5-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.5.0

* System was made a `package-inferred` and now uses Rove for tests.

<a id="x-28DEFMAIN-2FCHANGELOG-3A-3A-7C0-2E4-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.4.0

* Now [`defmain:defmain`][4130] handles all unhandled conditions, prints a backtrace
  and program exists with error code `1`.

Previously, programs just started debugger and if built with `CCL` it
  was impossible to interrupt it.
* Now it is possible to overwrite a short name of a parameter.
  For example:

`lisp
  (defmain main ((verbose "Show more informaition"
                          :flag t) ;; by default -v will be used
                 (version "Show version"
                          :flag t
                          :short "V"))
    ...)
`

Also, you can pass `NIL` as a value to disable short name:

  `lisp
  (defmain main ((verbose "Show more informaition"
                          :flag t) ;; by default -v will be used
                 (version "Show version"
                          :flag t
                          :short "V"))
    ...)
`

<a id="x-28DEFMAIN-2FCHANGELOG-3A-3A-7C0-2E3-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.3.0

* Fixed a way how `--help` option is processed. Now, this argument
  can be redefined by user, for example, to give this option another
  description.

* Now, if `:default "some string"` is given for option, then it will
  be parsed as string. Previously, `lispobj` type was used to parse
  value, and there were problems when you tried to pass as a value
  string with a colon, like some `URL`.

<a id="x-28DEFMAIN-2FCHANGELOG-3A-3A-7C0-2E2-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.2.0

* Added function [`defmain:print-help`][9b39].

* Fixed handling of existing `help` option. Now you can redefine it
  to give another description.

<a id="x-28DEFMAIN-2FCHANGELOG-3A-3A-7C0-2E1-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.1.0

* Number features here.

* Like that.

* Add new versions to the top.

* Specify dates as `2017-04-19`.

* Read [Keep`AC`hangelog.com][eeaa] for futher
  explanations.


[eeaa]: http://keepachangelog.com/
[3f72]: https://40ants.com/ci/
[4b6c]: https://40ants.com/defmain/#x-28DEFMAIN-3ADEFCOMMAND-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29
[4130]: https://40ants.com/defmain/#x-28DEFMAIN-3ADEFMAIN-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29
[86bc]: https://40ants.com/defmain/#x-28DEFMAIN-3AGET-SUBCOMMAND-NAME-20FUNCTION-29
[9b39]: https://40ants.com/defmain/#x-28DEFMAIN-3APRINT-HELP-20FUNCTION-29
[795a]: https://github.com/roswell/roswell

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
