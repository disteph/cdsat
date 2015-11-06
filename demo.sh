#!/bin/sh

function pause(){
   read -p "$*"
}

function lat(){
    cd latex
    pdflatex Main.tex
    cd ..
    xdg-open latex/Main.pdf &
    pause ""
}

function green () {
    echo -e "\033[1;32m$1\033[0m"
}

function test_psyche () {
    TEST_FILE=$1
    PSYCHE_PARAMS="-latex ${@:2} $TEST_FILE"
    $PAGER $TEST_FILE
    echo -e "  $(green "[") running \033[34m psyche $PSYCHE_PARAMS \033[1;32m]\033[0m"
    # ./main.native $PSYCHE_PARAMS
    echo "Finished computing. Converting proof to pdf."
    cd latex
    pdflatex Main.tex 2>&1 > /dev/null | sed "s/^/$(green [pdflatex])  /"
    PDFRETURNCODE=${PIPESTATUS[0]}
    cd ..
    echo ""
    [[ PDFRETURNCODE -eq 0 ]] && echo "You can see a pdf summary of the proof at ./latex/Main.pdf." || echo "Pdf compilation failed. You still can see the latex file at ./latex/Main.tex"
    echo ""
    pause "Press enter to continue."
}

test_psyche problems/DIMACS/UNSAT/test.cnf -pluginG dpll_wl
test_psyche problems/DIMACS/SatlibBench/hole7.cnf -pluginG dpll_wl
# test_psyche demo/LRA-test.smt2 -pluginG dpll_wl
test_psyche problems/QF_UF/hand_written/cc2.smt2 -pluginG dpll_wl
test_psyche problems/pelletier/p36.smt2 -pluginG hint
