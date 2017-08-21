=============================
         CDSAT,
Branch of the Psyche software
=============================


LICENSING

This program is free software; you can redistribute it and/or modify
it under the terms of the CeCILL-C FREE SOFTWARE LICENSE.

You should have received a copy of the CeCILL-C License with this
Kit, in the file named "LICENSE.txt".
If not, visit http://www.cecill.info


=========================
COMPILING AND INSTALLING:

In the main directory: run

  opam pin add cdsat-libs .
  oasis setup
  make cdsat build

The first line installs the CDSAT dependencies and the CDSAT libraries via opam.
The second configures the compilation, and the third compiles the executable locally.

For installation of the executable (optional):

  make install 

which installs the compiled executable at the location indicated at
configuration time (default is /usr/local/bin/). This step may require admin
roots.

For uninstallation:

  make uninstall

The CDSAT libraries can be removed from your system via opam:

  opam remove cdsat-libs

(note that opam packages on which CDSAT depended are still there.)


===================
RUNNING CDSAT:

If you did a local installation (without running 'make install'), a

  main.native

executable file has been created, at the root of the directory containing the
sources of Psyche.

If you did go through the installation (running 'make install'), you have a

  psyche

executable in the directory specified in your configuration (hopefully in your
path).

A command-line argument will be considered as the file name or directory name to treat.
If no argument is given, the standard input will be used.

Available options are:
  -no XXX forbids use of theory XXX
  -th XXX forces use of theory XXX
  -alltheories makes Psyche use all theories, except those specified by -no option
  -notheories makes Psyche use no theories, except those specified by -th option
  -plugin XXX selects XXX as the main plugin (among async)
  -parser XXX authorises XXX among the parsers to try (among dimacs, smtlib2)
  -latex allows latex output
  -alphasort treats input files in alphabetical order (default is from smaller to bigger)
  -skipsat skips instances expected to be sat
  -skipunsat skips instances expected to be unsat
  -skipunprovable skips instances expected to be unprovable
  -skipprovable skips instances expected to be provable
  -skipunknown skips instances without any result expectation
  -debug XXX iii prints on stdout those debug messages whose tags contain XXX and whose level is at most iii
  -step waits for keystroke after printing each debug message (applies to latest -debug)
  -nomemo disallows memoisation
  -version prints version and exits
  -help  Display this list of options
  --help  Display this list of options
