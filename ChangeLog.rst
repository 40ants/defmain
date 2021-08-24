===========
 ChangeLog
===========

0.12.1 (2021-08-24)
===================

* Added `(declare (ignorable %pop-argument))` to suppress a compiler `NOTE` under `SBCL`.

  Thanks to Alessio Stalla!

0.12.0 (2021-06-05)
===================

* Added LispWorks support.
* Program name now evaluated at runtime instead of a compile-time.

0.11.0 (2021-05-08)
===================

Backward incompatible change!
-----------------------------

Now the name of a function given to the
``DEFMAIN`` macro, should be a list, because it
can carry optional keyword arguments, such as
``:PROGRAM-NAME``.

Other changes
-------------
  
* Fixed subcommands for the case when a program was
  compiled using ``ASDF:MAKE``.
* Added more examples and improved documentation!

0.10.0 (2021-04-25)
===================

* Moved documentation from reStructured readme to 40ANTS-DOC
  documentation builder.
* Added github actions for building docs, linting and testing.
  Using `40ANTS-CI <https://40ants.com/ci/>`_ generator.

0.9.1 (2021-01-27)
==================

* Fixed work under ``ABCL`` and probably ``CMUCL``.
* Help commands now exit with 0 status code.
* Fixed annoying banner about missing ``CC`` env variable.

0.9.0 (2018-12-07)
==================

* Added support for programs, built with ``asdf:make``.

  Previously, only `roswell`_ was supported, but now
  you can define your system as:

  .. code:: common-lisp

     (defsystem work-hours
       :class :package-inferred-system
       :build-operation "program-op"
       :build-pathname "work-hours"
       :entry-point "work-hours/main:main"
       :depends-on ("work-hours/main"))

  And then call in the command line something like:

  .. code:: bash

     qlot exec ros run -L sbcl -e '(asdf:make :work-hours)'


0.8.0 (2018-10-15)
==================

* Added support for positional arguments.
* Fixed issue of catching signals not inherited from ``error`` and
  handling them as errors by printing traceback and quitting.

0.7.2 (2018-07-14)
==================

Added Ctrl-C handling. However seems it does not work for CCL :(

0.7.1 (2018-06-15)
==================

Error ``Undeclared free variable DEFMAIN/DEFMAIN::HELP-COMMANDS`` was
fixed for cases where there is no ``defcommand`` calls beside ``defmain``.

0.7.0 (2018-06-13)
==================

Defmain macro now restores temporary pathnames inside of ``cl-fad`` and
``uiop`` packages by setting ``logical-pathname-translations`` for
``"TEMPORARY-FILES"`` and by calling
``(uiop:setup-temporary-directory)``.

This solves issues in programs which use ``(uiop:run-program ...)`` or
``(cl-fad:with-output-to-temporary-file ...)`` for example.

0.6.1 (2018-06-08)
==================

* Now function ``get-subcommand-name`` is available in the main function
  if your specified ``&subcommand`` as it's argument.

0.6.0 (2018-06-07)
==================

* Added support for nested commands defined with ``defcommand`` macro.

0.5.0 (2018-06-02)
==================

* System was made a ``package-inferred`` and now uses Rove for tests.

0.4.0 (2018-05-18)
==================

* Now ``defmain`` handles all unhandled conditions, prints a backtrace
  and program exists with error code ``1``.

  Previously, programs just started debugger and if built with CCL it
  was impossible to interrupt it.
* Now it is possible to overwrite a short name of a parameter.
  For example:

  .. code:: lisp

     (defmain main ((verbose "Show more informaition"
                             :flag t) ;; by default -v will be used
                    (version "Show version"
                             :flag t
                             :short "V"))
       ...)

  Also, you can pass ``nil`` as a value to disable short name:
  
  .. code:: lisp

     (defmain main ((verbose "Show more informaition"
                             :flag t) ;; by default -v will be used
                    (version "Show version"
                             :flag t
                             :short "V"))
       ...)

0.3.0 (2018-05-12)
==================

* Fixed a way how ``--help`` option is processed. Now, this argument
  can be redefined by user, for example, to give this option another
  description.
* Now, if ``:default "some string"`` is given for option, then it will
  be parsed as string. Previously, ``lispobj`` type was used to parse
  value, and there were problems when you tried to pass as a value
  string with a colon, like some URL.

0.2.0 (2018-02-09)
==================

* Added function ``print-help``.
* Fixed handling of existing ``help`` option. Now you can redefine it
  to give another description.

0.1.0 (unreleased)
==================

* Number features here.
* Like that.
* Add new versions to the top.
* Specify dates as ``2017-04-19``.
* Read `KeepAChangelog.com <http://keepachangelog.com/>`_ for futher
  explanations.


.. _roswell: https://github.com/roswell/roswell
