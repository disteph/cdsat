#!/bin/bash

function pause(){
   read -p "$*"
}

function lat(){
    cd latex
    pdflatex Main.tex
    cd ..
    evince latex/Main.pdf &
    pause ""
}

echo "psyche -theory empty -examples -gplugin naive -latex"
pause ""
./main.native -gplugin naive -theory empty -examples -latex
pause ""
lat
echo "psyche -theory empty -examples -latex"
pause ""
./main.native -theory empty -examples -latex
pause ""
lat
emacs demo/UNSAT-test.cnf
echo "psyche -gplugin dpll_wl -latex demo/UNSAT-test.cnf"
pause ""
./main.native -gplugin dpll_wl -latex demo/UNSAT-test.cnf
pause "finished saving"
cd latex
pdflatex Main.tex
pause ""
cd ..
emacs latex/output.tex
pause ""
emacs demo/hole7.cnf
echo "psyche -gplugin dpll_wl demo/hole7.cnf"
pause ""
./main.native -gplugin dpll_wl demo/hole7.cnf
pause ""
emacs demo/LRA-test.smt2
echo "psyche -gplugin dpll_wl -latex demo/LRA-test.smt2"
pause ""
./main.native -gplugin dpll_wl -latex demo/LRA-test.smt2
pause ""
lat
emacs demo/cc2.smt2
echo "psyche -latex demo/cc2.smt2"
pause ""
./main.native -latex demo/cc2.smt2
pause ""
echo "psyche  -gplugin hint -latex problems/pelletier/p36.smt2  "
pause ""
./main.native -gplugin hint -latex problems/pelletier/p36.smt2  
pause ""
lat
