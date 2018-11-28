# Tests for lemma_forgetting strategy through different variables
# Need the library subprocess

import os
import subprocess
import numpy as np
import matplotlib.pyplot as plt

fileTest = "tests/DIMACS/UNSAT/"

# Check current working directory.
#retval = os.getcwd()
#print("Current working directory %s" % retval)

# Change the directory
# os.chdir(path)

# Check current working directory.
#retval = os.getcwd()
#print ("Directory changed successfully %s" % retval)

#os.system("./main.native "+file)

def parse(line, n):
    tableline = line.split("===========================\n")
    result = []
    for i in range(n) :
        result.append(parseblock(tableline[i+1]))
    return result;

def parseblock(block):
    tableBlock = block.split("\n")
    nameFile = tableBlock[0].split(" ")[-1].split("/")[-1]
    time = tableBlock[3].split(" ")[-1]
    nbDec = tableBlock[4].split(" ")[-1]
    Decpersec = tableBlock[5].split(" ")[-1]
    OptionsPerDec = tableBlock[6].split(" ")[-1]
    return nameFile+" "+time+" "+nbDec+" "+Decpersec+" "+OptionsPerDec


nbFiles = subprocess.Popen(["ls", fileTest], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
nbFiles = (nbFiles.stdout.read()).decode("utf-8").split("\n")
nbFiles = len(nbFiles)-1

fileWrite = "data4.txt"
fichier = open("lemmas_forgetting_tests/"+fileWrite, "a")

start = 800
end = 3000
steps = 1000

fichier.write("Number of files : "+str(nbFiles)+"\nNumber of steps : "+str(steps)+"\n")
fichier.write("File name / Execution time / Number of decisions / Decisions per second / Options per decision\n")


for i in np.linspace(start, end, steps) :
    print ("step "+str(int(((i-start)/(end-start))*steps-0.00001)+1)+" over "+str(steps))
    fichier.write("===========================\n")
    fichier.write("Increment="+str(i)+"\n")
    x = subprocess.Popen(["./main.native", "-lemmasmax", str(int(i)), "tests/DIMACS/UNSAT/"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    line = x.stdout.read().decode("utf-8")
    ex = parse(line, nbFiles)
    for j in range(0, len(ex)) :
        fichier.write(ex[j]+"\n")


fichier.close()

