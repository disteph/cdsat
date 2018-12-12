# Tests for lemma_forgetting strategy through two different variables
# Need the library subprocess
# Need to be launched from the directory cdsat.

import os
import subprocess
import numpy as np
import re
import math
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
        #print(tableline)
        result.append(parseblock(tableline[i+1]))
    return result;

def parseblock(block):
    tableBlock = block.split("\n")
    #print(tableBlock)
    nameFile = tableBlock[0].split(" ")[-1].split("/")[-1]
    time = tableBlock[3].split(" ")[-1]
    nbDec = tableBlock[4].split(" ")[-1]
    Decpersec = tableBlock[5].split(" ")[-1]
    OptionsPerDec = tableBlock[6].split(" ")[-1]
    return time+" "+nbDec+" "+Decpersec+" "+OptionsPerDec

def getLemmasQuantity(fileentry) :
    print("Getting number of lemmas without forgetting them")
    x=subprocess.Popen(["./main.native", "-lemmasstep", "1000000", "-debug", "lemmasQuantity", "0", "unsat_tests/"+fileentry], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    for i in range(4) :
        x.stdout.readline()
    line1=x.stdout.readline().decode("utf-8")
    line2=x.stdout.readline().decode("utf-8")
    while (line2.split(" ")[0]=="Number") :
        line1=line2
        line2=x.stdout.readline().decode("utf-8")
    return int(line1.split(" : ")[-1])
     


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
            #print(lineTable)
            result.append(lineTable[2])
            result.append(lineTable[3])
    fileToRead.close()
    return result
    

def main(fileentry, fileWrite):
    
    
    print("Test for "+fileentry+"\n\n")
    
    #nbFiles = subprocess.Popen(["ls", fileTest], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    #nbFiles = (nbFiles.stdout.read()).decode("utf-8").split("\n")
    #nbFiles = len(nbFiles)-1
    
    fichier = open("lemmas_forgetting_tests/dataMap_bddTests/"+fileWrite, "a")
    
    
    nbLemmas = getLemmasQuantity(fileentry)
    
    #startCount = 50
    #endCount = 800
    #stepsCount = 16
    
    #startIncr = 1.1
    #endIncr = 2.
    #stepsIncr = 10
    
    #startDecay = 1.02
    #endDecay = 1.1
    #stepsDecay = 5
    
    
    #linspaceIncr = np.linspace(startIncr, endIncr, stepsIncr)
    #linspaceDecay = np.linspace(startDecay, endDecay, stepsDecay)
    
    stepArray=[int(math.exp(((9-i)*math.log(nbLemmas/25)+i*math.log(nbLemmas))/9.)) for i in range(10)]
    decayArray=[0.02, 0.05, 0.1, 0.25, 0.5]
    incrArray =[math.exp(((9-i)*math.log(1/math.log(nbLemmas))+i*math.log(36))/9.) for i in range(10)]
    
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
    
    fichier.write("File "+fileentry+"\n")
    fichier.write("Number of variables="+str(nbVar)+"\n")
    fichier.write("Number of clauses="+str(nbClauses)+"\n")
    fichier.write("With argument keeplemmas - execution time : "+str(moyenne)+"\n")
    fichier.write("Number of lemmas : "+str(nbLemmas)+"\n")
    fichier.write("Number of stepsCount : 10\nNumber of stepsIncr : 10\nNumber of stepsDecay : 5\n")
    
    fichier.write("Execution time / Number of decisions / Decisions per second / Options per decision\n")
    iStep =0
    for i in stepArray :
        iStep=iStep+1
        print ("StepMaxCount: "+str(iStep)+" over "+str(len(stepArray))+"\n********")
        fichier.write("***\n")
        fichier.write("MaxCount="+str(max(i, 1))+"\n")
        kStep=0
        for k in decayArray :
            kStep=kStep+1
            decayParam=1+k
            print("Decay step "+str(kStep)+" over "+str(len(decayArray))+"\n")
            fichier.write("@@@\n")
            fichier.write("Decay="+str(decayParam)+"\n")
            jStep=0
            for j in incrArray : 
                jStep=jStep+1
                incrParam=j*k+1
                print ("StepIncr: "+str(jStep)+" over "+str(len(incrArray)))
                fichier.write("===========================\n")
                fichier.write("Increment="+str(incrParam)+"\n")
                #print("Command "+"./main.native "+ "-lemmasstep "+ str(int(max(i, 1)))+ " -lemmasincrmt "+ str(incrParam)+ " -lemmasdecay "+ str(decayParam),  " unsat_tests/"+fileentry)
                x=popen_timeout(["./main.native", "-lemmasstep", str(int(max(i, 1))), "-lemmasincrmt", str(incrParam), "-lemmasdecay", str(decayParam),  "unsat_tests/"+fileentry], timeout)
                if (x!=False) :
                    line = x.stdout.read().decode("utf-8")
                    #print(line)
                    ex = parse(line, 1)
                    for l in range(0, len(ex)) :
                        fichier.write(ex[l]+"\n")
                else :
                    fichier.write("XXX Execution time too long : greater than "+str(timeout)+" sec\n")
    
    
    fichier.close()
    
x = subprocess.Popen(["ls", "unsat_tests"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
y = x.stdout.read().decode("utf-8")
filesTests = y.split("\n")

i=1
for fileentry in filesTests :
    print("File  "+str(i)+" over "+str(len(filesTests)))
    if fileentry != "" and fileentry!=" " and i>2:
        main(fileentry, "dataMap"+fileentry+".txt")
    print('\n\n')
    i=i+1
