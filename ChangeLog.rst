===========
 ChangeLog
===========

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
