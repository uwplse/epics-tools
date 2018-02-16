# EPICS Static Analysis Tools

[![Build Status](https://travis-ci.org/uwplse/epics-tools.svg?branch=master)](https://travis-ci.org/uwplse/epics-tools)

This repository hosts a linter and symbolic interpreter for
[EPICS](https://epics.anl.gov/) applications.

To get started, run

    $ make

The Makefile first checks to ensure that you have installed all necessary
prerequisites.  If that step fails, you may need to install the missing
software.  If you need to circumvent the checks, run `touch check.ok` and
then try again.

The compiled tools are placed in a new folder called `build`.  For information
on usage, run `make doc` and consult `doc/symbolic-interpreter.pdf`.
