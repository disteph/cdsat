Compiling the source (requires autoconf installed):

autoconf
./configure
make opt

(this process reads configure.in and Makefile.in)


===================
Running the programme

./EureCaml.opt

or

./run.sh
(runs EureCaml.opt then pdflatex then acroread)

=====================
With OASIS:

Compiling:

ocaml setup.ml -configure
ocaml setup.ml -build

Running the program:

_build/EuroCaml

or

EuroCaml

if the program is installed (see below)

Installing the program (as root):

ocaml setup.ml -install


