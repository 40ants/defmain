# Building

To build this example, you'll need a [Roswell](https://github.com/roswell/roswell),
because it allow to use any Common Lisp implementation and easily switch between them.

Also, Roswell setup Quicklisp so you can make `(ql:quickload ...)` without downloading
and setup Quicklisp by hand.

To build program using `ASDF:MAKE`, just do:

    cd simple
    make

Also, you can use Roswell script:

    ./roswellexample.ros --help

or build it into a binary:

    ros build defmain-example.ros
