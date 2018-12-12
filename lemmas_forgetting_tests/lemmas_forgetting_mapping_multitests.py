# Tests for lemma_forgetting strategy through two different variables
# Need the library subprocess
# Need to be launched from the directory cdsat.

import os
import subprocess
import numpy as np
import re
from time import sleep



fileTest = "tests/DIMACS/UNSAT/dubois100.cnf"

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
    return time+" "+nbDec+" "+Decpersec+" "+OptionsPerDec

# Comes from https://www.gungorbudak.com/blog/2015/08/30/simple-way-of-pythons-subprocesspopen/
def popen_timeout(command, timeout):
    p = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    for t in range(timeout):
        sleep(1)
        if p.poll() is not None:
            return p
    p.kill()
    return False


def getVarsClauses(fileentry) :
    result=[]
    
    fileToRead=open("unsat_tests/"+fileentry, "r")
    for line in fileToRead :
        if line[0]=='p' :
            lineTable=re.compile("\s{0,}").split(line)
            print(lineTable)
            result.append(lineTable[2])
            result.append(lineTable[3])
    fileToRead.close()
    return result
    

def main(fileentry, fileWrite):
    
    
    print("Test for "+fileentry+"\n\n")
    
    #nbFiles = subprocess.Popen(["ls", fileTest], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    #nbFiles = (nbFiles.stdout.read()).decode("utf-8").split("\n")
    #nbFiles = len(nbFiles)-1
    
    fichier = open("lemmas_forgetting_tests/dataMap_multitests_withDecay/"+fileWrite+"tessssssst", "a")
    
    startCount = 50
    endCount = 800
    stepsCount = 16
    
    startIncr = 1.1
    endIncr = 2.
    stepsIncr = 10
    
    startDecay = 1.02
    endDecay = 1.1
    stepsDecay = 5
    
    nbVarClauses = getVarsClauses(fileentry)
    nbVar=nbVarClauses[0]
    nbClauses=nbVarClauses[1]
    
    
    nbFirstTry = 5
    firstTryextraction=[]
    for i in range(nbFirstTry) :
        print("Trying without forgetting : "+str(i+1)+"/"+str(nbFirstTry))
        x=subprocess.Popen(["./main.native", "-keeplemmas", "unsat_tests/"+fileentry], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        line = x.stdout.read().decode("utf-8")
        ex = parse(line, 1)
        firstTryextraction.append(ex[0].split(" ")[0])
    
    sum = 0;
    for i in firstTryextraction :
        sum+=float(i)
    moyenne=sum/nbFirstTry;
    
    timeout=int(3*moyenne)+1
    
    linspaceIncr = np.linspace(startIncr, endIncr, stepsIncr)
    linspaceDecay = np.linspace(startDecay, endDecay, stepsDecay)
    fichier.write("File "+fileentry+"\n")
    fichier.write("Number of variables="+str(nbVar)+"\n")
    fichier.write("Number of clauses="+str(nbClauses)+"\n")
    fichier.write("With argument keeplemmas - execution time : "+str(moyenne)+"\n")
    fichier.write("Number of files : "+str(1)+"\nNumber of stepsCount : "+str(stepsCount)+"\nNumber of stepsIncr : "+str(stepsIncr)+"\nNumber of stepsDecay : "+str(stepsDecay)+"\n")
    
    fichier.write("File name / Execution time / Number of decisions / Decisions per second / Options per decision\n")
    
    for i in np.linspace(startCount, endCount, stepsCount) :
        print ("StepMaxCount: "+str(int(((i-startCount)/(endCount-startCount))*stepsCount-0.00001)+1)+" over "+str(stepsCount)+"\n********")
        fichier.write("***\n")
        fichier.write("MaxCount="+str(i)+"\n")
        for k in linspaceDecay :
            print("Decay step "+str(1+int(stepsDecay*(k-1.)/(endDecay-1.)-0.000001))+" over "+str(stepsDecay)+"\n")
            fichier.write("@@@\n")
            fichier.write("Decay="+str(k)+"\n")
            for j in linspaceIncr : 
                print ("StepIncr: "+str(int(((j-startIncr)/(endIncr-startIncr))*stepsIncr-0.00001)+1)+" over "+str(stepsIncr))
                fichier.write("===========================\n")
                fichier.write("Increment="+str(j)+"\n")
                x=popen_timeout(["./main.native", "-lemmasstep", str(int(i)), "-lemmasincrmt", str(int(j)), "-lemmasdecay", str(k),  "unsat_tests/"+fileentry], timeout)
                if (x!=False) :
                    line = x.stdout.read().decode("utf-8")
                    ex = parse(line, 1)
                    for l in range(0, len(ex)) :
                        fichier.write(ex[l]+"\n")
                else :
                    fichier.write("XXX Execution time too long : greater than "+str(timeout)+" sec\n")
            print("")
    
    
    fichier.close()
    
x = subprocess.Popen(["ls", "unsat_tests"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
y = x.stdout.read().decode("utf-8")
filesTests = y.split("\n")

i=1
for fileentry in filesTests :
    print("File  "+str(i)+" over "+str(len(filesTests)))
    if fileentry != "" and fileentry!=" ":
        main(fileentry, "dataMap"+fileentry+".txt")
    print('\n\n')
    i=i+1
