# Making the graph for MaxCoutn and Increment step variation
# Need to be launched from the directory cdsat.

import os
import numpy as np
import matplotlib.pyplot as plt
import math
import subprocess
from sklearn.linear_model import LinearRegression


##FUNCTION FROM lemmas_forgetting_multitests.py
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
###END

def getStepCount(data, i) :
    return data[i][0]

def getDecay(data, i, j) :
    return data[i][1+j][0]

def getIncrement(data, i, j, k):
    return data[i][1+j][1+k][0]
    
def getExecutionTime(data, i, j, k):
    return data[i][1+j][1+k][1]
    
def getBestExecutionTime(data, stepsCount, stepsDecay, stepsIncr) :
    minExec = getExecutionTime(data, 0, 0, 0)
    ibest=0
    jbest=0
    kbest=0
    for i in range(stepsCount) :
        for j in range(stepsDecay):
            for k in range(stepsIncr) :
                execTime = getExecutionTime(data, i, j, k)
                if execTime<minExec :
                    minExec=execTime
                    ibest=i
                    jbest=j
                    kbest=k
    
    return (minExec, getStepCount(data, ibest), getDecay(data, ibest, jbest), getIncrement(data, ibest, jbest, kbest), ibest, jbest, kbest)

def getBestStepCount(data, stepsCount, stepsDecay, stepsIncr) :
    return getBestExecutionTime(data, stepsCount, stepsDecay, stepsIncr)[1]

def getBestDecay(data, stepsCount, stepsDecay, stepsIncr) :
    return getBestExecutionTime(data, stepsCount, stepsDecay, stepsIncr)[2]

def getBestIncrement(data, stepsCount, stepsDecay, stepsIncr) :
    return getBestExecutionTime(data, stepsCount, stepsDecay, stepsIncr)[3]



#Parse two lines from dataMap texts, that is the text between two "==========="
# That is : first, the value of Increment and second, one line whose sense is given by the line "Execution time / Number of decisions / Decisions per second / Options per decision"
def parseLines(lines):
    table=lines.split("\n")
    data=[float(table[0].split("=")[-1])]
    if (table[1][0]=='X') :
        data.append(float(table[1].split("than ")[1].split(" sec")[0]))
    else :
        data.append(float(table[1].split(" ")[0]))
    return data

#Parse a block, that is the text between two "@@@"
def parseBlock3(block3):
    block4 = block3.split("===========================\n")
    data=[float(block4[0].split("=")[-1].split("\n")[0])]
    for k in range(1, len(block4)):
        data.append(parseLines(block4[k]))
    return data

#Parse a block between which is the text between two "***"

def parseBlock2(block2):
    block3 = block2.split("@@@\n")
    data=[int(block3[0].split("=")[-1].split("\n")[0])]
    for j in range(1, len(block3)):
        data.append(parseBlock3(block3[j]))
    return data

#Parse the whole text after the header
def parseBlock1(block) :
    block2 = block.split("***\n")
    data=[]
    for i in range(1, len(block2)):
        data.append(parseBlock2(block2[i]))
    return data


#Parse the whole file
#The file must look like this :
# Name of the file
# Number of variables
# Number of clauses
# Execution time with the tag -keeplemmas
# Number of learned lemmas without forgetting one lemma
# Number of arguments for the tag -lemmasstep
# Number of arguments for the tag -lemmasincrmt
# Number of arguments for the tag -lemmasdecay
# Then, all the blocks where the parameters MaxCount, Decay and Increment appear. The form of data is the following :
# data=[[MaxCount, [Decay, [Increment, Execution time], ...], ...], ...]
def parseFile(file):
    line = file.readline()
    nameFile=line.split(" ")[-1].split("\n")[0]
    line = file.readline()
    nbVars = int(line.split("=")[-1].split("\n")[0])
    line = file.readline()
    nbClauses = int(line.split("=")[-1].split("\n")[0])
    line = file.readline()
    averageKeepLemmas = float(line.split(" ")[-1].split("\n")[0])
    line = file.readline()
    nbLemmas=int(line.split(" ")[-1].split("\n")[0])
    line = file.readline()
    stepsCount = int(line.split(" ")[-1].split("\n")[0])
    line = file.readline()
    stepsIncr = int(line.split(" ")[-1].split("\n")[0])
    line = file.readline()
    stepsDecay = int(line.split(" ")[-1].split("\n")[0])
    
    text = file.read()
    data = parseBlock1(text)
    
    return (nameFile, nbVars, nbClauses, averageKeepLemmas, nbLemmas, stepsCount, stepsIncr, stepsDecay, data)
    
def RegressionSteps(filesArray):
    
    #initialise lists
    Xlist = []
    Ylist = []
    for fileRead in filesArray :
        fichier=open("lemmas_forgetting_tests/dataMap_bddTests/"+fileRead, "r")
        parsing = parseFile(fichier)
        nbVars = parsing[1]
        nbClauses = parsing[2]
        if nbVars<1000 :
            Xlist.append([nbVars, nbClauses])
    
            stepsCount=parsing[5]
            stepsIncr=parsing[6]
            stepsDecay=parsing[7]
            data=parsing[8]
            
            Ylist.append(getBestStepCount(data, stepsCount, stepsDecay, stepsIncr))
        
        fichier.close()
        
    X=np.array(Xlist)
    Y=np.array(Ylist)
    
    VarArray = np.dot(X, np.array([1, 0]))
    VarArray=VarArray.reshape(Y.size, -1)
    regVar= LinearRegression().fit(VarArray, Y)
    score = regVar.score(VarArray, Y)
    coef = regVar.coef_
    intercept = regVar.intercept_
    y_predicted = intercept + coef*VarArray
    
    plt.figure(1)
    plt.scatter(VarArray, Y, label="Variables")
    plt.plot(VarArray, y_predicted, 'k-')
    plt.xlabel("Variables")
    plt.ylabel("StepsCount optimisé")
    plt.text(np.amax(VarArray)/2, 5, "Regression score : "+str(score))
    plt.legend(loc='best')
    plt.show()
    
    clausesArray=np.dot(X, np.array([0, 1]))
    clausesArray=clausesArray.reshape(Y.size, -1)
    regClause=LinearRegression().fit(clausesArray, Y)
    y_predicted = regClause.intercept_ + regClause.coef_*clausesArray
    plt.figure(2)
    plt.scatter(clausesArray, Y, label="Clauses")
    plt.plot(clausesArray, y_predicted, 'k')
    plt.xlabel("Clauses")
    plt.ylabel("StepsCount optimisé")
    plt.legend(loc='best')
    plt.show()
    
    print(score)
    #print(coef)
    #print(intercept)
    
    return score, coef, intercept, regVar
    
def RegressionDecay(filesArray):
    
    #initialise lists
    Xlist = []
    Ylist = []
    for fileRead in filesArray :
        fichier=open("lemmas_forgetting_tests/dataMap_bddTests/"+fileRead, "r")
        parsing = parseFile(fichier)
        nbVars = parsing[1]
        nbClauses = parsing[2]
        if nbVars<1000 :
            Xlist.append([nbVars, nbClauses])
    
            stepsCount=parsing[5]
            stepsIncr=parsing[6]
            stepsDecay=parsing[7]
            data=parsing[8]
            
            Ylist.append(getBestDecay(data, stepsCount, stepsDecay, stepsIncr)-1)
        
        fichier.close()
        
    X=np.array(Xlist)
    Y=np.array(Ylist)    
    VarArray = np.dot(X, np.array([1, 0]))
    VarArray=VarArray.reshape(Y.size, -1)
    regVar= LinearRegression().fit(VarArray, Y)
    score = regVar.score(VarArray, Y)
    coef = regVar.coef_
    intercept = regVar.intercept_
    y_predicted = intercept + coef*VarArray
    
    plt.figure(3)
    plt.scatter(VarArray, Y, label="Variables")
    plt.plot(VarArray, y_predicted, 'k-')
    plt.xlabel("Variables")
    plt.ylabel("Decay optimisé")
    plt.text(np.amax(VarArray)/2, 1.5, "Regression score : "+str(score))
    plt.legend(loc='best')
    plt.show()
    
    clausesArray=np.dot(X, np.array([0, 1]))
    clausesArray=clausesArray.reshape(Y.size, -1)
    regClause=LinearRegression().fit(clausesArray, Y)
    y_predicted = regClause.intercept_ + regClause.coef_*clausesArray
    plt.figure(4)
    plt.scatter(clausesArray, Y, label="Clauses")
    plt.plot(clausesArray, y_predicted, 'k')
    plt.xlabel("Clauses")
    plt.ylabel("Decay optimisé")
    plt.legend(loc='best')
    plt.show()
    
    print(score)
    #print(coef)
    #print(intercept)
    
    return score, coef, intercept, regVar

def RegressionIncrement(filesArray):
    
    #initialise lists
    Xlist = []
    Ylist = []
    for fileRead in filesArray :
        fichier=open("lemmas_forgetting_tests/dataMap_bddTests/"+fileRead, "r")
        parsing = parseFile(fichier)
        nbVars = parsing[1]
        nbClauses = parsing[2]
        if nbVars<1000 :
            Xlist.append([nbVars, nbClauses])
    
            stepsCount=parsing[5]
            stepsIncr=parsing[6]
            stepsDecay=parsing[7]
            data=parsing[8]
            
            Ylist.append(getBestIncrement(data, stepsCount, stepsDecay, stepsIncr)-1)
            
        fichier.close()
        
    X=np.array(Xlist)
    Y=np.array(Ylist)    
    
    VarArray = np.dot(X, np.array([1, 0]))
    VarArray=VarArray.reshape(Y.size, -1)
    regVar= LinearRegression().fit(VarArray, Y)
    score = regVar.score(VarArray, Y)
    coef = regVar.coef_
    intercept = regVar.intercept_
    y_predicted = intercept + coef*VarArray
    
    plt.figure(5)
    plt.scatter(VarArray, Y, label="Variables")
    plt.plot(VarArray, y_predicted, 'k-')
    plt.xlabel("Variables")
    plt.ylabel("Increment optimisé")
    plt.text(np.amax(VarArray)/2, 5, "Regression score : "+str(score))
    plt.legend(loc='best')
    plt.show()
    
    clausesArray=np.dot(X, np.array([0, 1]))
    clausesArray=clausesArray.reshape(Y.size, -1)
    regClause=LinearRegression().fit(clausesArray, Y)
    y_predicted = regClause.intercept_ + regClause.coef_*clausesArray
    plt.figure(6)
    plt.scatter(clausesArray, Y, label="Clauses")
    plt.plot(clausesArray, y_predicted, 'k')
    plt.xlabel("Clauses")
    plt.ylabel("Increment optimisé")
    plt.legend(loc='best')
    plt.show()
    
    print(score)
    #print(coef)
    #print(intercept)
    
    return score, coef, intercept, regVar

def Speedup1(filesArray) :
    #initialise lists
    Xlist = []
    Ylist = []
    for fileRead in filesArray :
        fichier=open("lemmas_forgetting_tests/dataMap_bddTests/"+fileRead, "r")
        parsing = parseFile(fichier)
        nbVars = parsing[1]
        nbClauses = parsing[2]
        Xlist.append([nbVars, nbClauses])
        averageKeepLemmas=parsing[3]
        stepsCount=parsing[5]
        stepsIncr=parsing[6]
        stepsDecay=parsing[7]
        data=parsing[8]
        
        Ylist.append((averageKeepLemmas-getBestExecutionTime(data, stepsCount, stepsDecay, stepsIncr)[0])/averageKeepLemmas)
        
        fichier.close()
    
    X=np.array(Xlist)
    VarArray = np.dot(X, np.array([1, 0]))
    Y=np.array(Ylist)
    
    
    plt.figure(7)
    plt.scatter(VarArray, Y)
    plt.xlabel("Number of Variables")
    plt.ylabel("Speedup")
    plt.show()
    
def Speedup2(filesArray) :
    #initialise lists
    Xlist = []
    Ylist = []
    lemmasstepReg = RegressionSteps(filesArray)[-1]
    lemmasdecayReg = RegressionDecay(filesArray)[-1]
    lemmasincrmtReg = RegressionIncrement(filesArray)[-1]
    nbFilesCount=0
    for fileRead in filesArray :
        nbFilesCount+=1
        fichier=open("lemmas_forgetting_tests/dataMap_bddTests/"+fileRead, "r")
        parsing = parseFile(fichier)
        nbVars = parsing[1]
        nbClauses=parsing[2]
        Xlist.append(nbVars)
        
        averageKeepLemmas=parsing[3]
        stepsCount=parsing[5]
        stepsIncr=parsing[6]
        stepsDecay=parsing[7]
        data=parsing[8]
        
        lemmasstep=min(lemmasstepReg.predict(nbVars)[0], nbClauses/2)
        lemmasincrmt=lemmasincrmtReg.predict(nbVars)[0]
        lemmasdecay=lemmasdecayReg.predict(nbVars)[0]
       
        fileExec = fileRead.split("dataMap")[1].split(".txt")[0]
        print("Processing file "+fileExec+" : "+str(nbFilesCount)+" over "+str(len(filesArray)))
        x = subprocess.Popen(["./main.native", "-lemmasstep", str(int(max(1, lemmasstep))), "-lemmasincrmt", str(lemmasincrmt), "-lemmasdecay", str(lemmasdecay),  "unsat_tests/"+fileExec], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        result = x.stdout.read().decode("utf-8")
        ex = parse(result, 1)
        time = float(ex[0].split(" ")[0])
        
        Ylist.append((averageKeepLemmas-time)/averageKeepLemmas)
        
        fichier.close()
    
    X=np.array(Xlist)
    Y=np.array(Ylist)
    
    plt.figure(8)
    plt.scatter(X, Y)
    plt.xlabel("Number of Variables")
    plt.ylabel("Speedup")
    plt.show()

def bestDecay(filesArray):
    Ylist = []
    for fileRead in filesArray :
        fichier=open("lemmas_forgetting_tests/dataMap_bddTests/"+fileRead, "r")
        parsing = parseFile(fichier)
        
        stepsCount=parsing[5]
        stepsIncr=parsing[6]
        stepsDecay=parsing[7]
        data=parsing[8]
        
        Ylist.append(getBestExecutionTime(data, stepsCount, stepsDecay, stepsIncr)[2])
    
    Y=np.array(Ylist)
    plt.hist(Y)
    plt.show()



def getBestExecutionTimeFixeddecay(data, stepsCount, stepsIncr, indexDecay) :
    minExec = getExecutionTime(data, 0, 0, 0)
    ibest=0
    kbest=0
    for i in range(stepsCount) :
        for k in range(stepsIncr) :
            execTime = getExecutionTime(data, i, indexDecay, k)
            if execTime<minExec :
                minExec=execTime
                ibest=i
                kbest=k
    
    return (minExec, getStepCount(data, ibest), getIncrement(data, ibest, indexDecay, kbest), ibest, kbest)

def bestEta(filesArray, indexDecay):
    Ylist = []
    for fileRead in filesArray :
        fichier=open("lemmas_forgetting_tests/dataMap_bddTests/"+fileRead, "r")
        parsing = parseFile(fichier)
        
        stepsCount=parsing[5]
        stepsIncr=parsing[6]
        data=parsing[8]
        
        Ylist.append(getBestExecutionTimeFixeddecay(data, stepsCount, stepsIncr, indexDecay)[2])
    print(Ylist)
    Y=np.array(Ylist)
    plt.hist(Y, 15)
    plt.show()

def RegressionStepsFixeddecay(filesArray, indexDecay):
    #initialise lists
    Xlist = []
    Ylist = []
    for fileRead in filesArray :
        fichier=open("lemmas_forgetting_tests/dataMap_bddTests/"+fileRead, "r")
        parsing = parseFile(fichier)
        nbVars = parsing[1]
        nbClauses = parsing[2]
        if nbVars<1000 :
            Xlist.append([nbVars, nbClauses])
    
            stepsCount=parsing[5]
            stepsIncr=parsing[6]
            data=parsing[8]
            
            Ylist.append(getBestExecutionTimeFixeddecay(data, stepsCount, stepsIncr, indexDecay)[2])
            
        fichier.close()
        
    X=np.array(Xlist)
    Y=np.array(Ylist)    
    
    VarArray = np.dot(X, np.array([1, 0]))
    VarArray=VarArray.reshape(Y.size, -1)
    regVar= LinearRegression().fit(VarArray, Y)
    score = regVar.score(VarArray, Y)
    coef = regVar.coef_
    intercept = regVar.intercept_
    y_predicted = intercept + coef*VarArray
    
    plt.figure(9)
    plt.scatter(VarArray, Y, label="Variables")
    plt.plot(VarArray, y_predicted, 'k-')
    plt.xlabel("Variables")
    plt.ylabel("Increment optimisé (decay fixé)")
    plt.text(np.amax(VarArray)/2, 1, "Regression score : "+str(score))
    plt.legend(loc='best')
    plt.show()
    
    clausesArray=np.dot(X, np.array([0, 1]))
    clausesArray=clausesArray.reshape(Y.size, -1)
    regClause=LinearRegression().fit(clausesArray, Y)
    y_predicted = regClause.intercept_ + regClause.coef_*clausesArray
    plt.figure(10)
    plt.scatter(clausesArray, Y, label="Clauses")
    plt.plot(clausesArray, y_predicted, 'k')
    plt.xlabel("Clauses")
    plt.ylabel("Increment optimisé (decay fixé)")
    plt.legend(loc='best')
    plt.show()
    
    print(score)
    #print(coef)
    #print(intercept)
    
    return score, coef, intercept, regVar
    
    

def RegressionIncrementFixeddecay(filesArray, indexDecay) :
    #initialise lists
    Xlist = []
    Ylist = []
    for fileRead in filesArray :
        fichier=open("lemmas_forgetting_tests/dataMap_bddTests/"+fileRead, "r")
        parsing = parseFile(fichier)
        nbVars = parsing[1]
        nbClauses = parsing[2]
        if nbVars<1000 :
            Xlist.append([nbVars, nbClauses])
    
            stepsCount=parsing[5]
            stepsIncr=parsing[6]
            data=parsing[8]
            
            Ylist.append(getBestExecutionTimeFixeddecay(data, stepsCount, stepsIncr, indexDecay)[3])
            
        fichier.close()
        
    X=np.array(Xlist)
    Y=np.array(Ylist)    
    
    VarArray = np.dot(X, np.array([1, 0]))
    VarArray=VarArray.reshape(Y.size, -1)
    regVar= LinearRegression().fit(VarArray, Y)
    score = regVar.score(VarArray, Y)
    coef = regVar.coef_
    intercept = regVar.intercept_
    y_predicted = intercept + coef*VarArray
    
    plt.figure(11)
    plt.scatter(VarArray, Y, label="Variables")
    plt.plot(VarArray, y_predicted, 'k-')
    plt.xlabel("Variables")
    plt.ylabel("Increment optimisé (decay fixé)")
    plt.text(np.amax(VarArray)/2, 1, "Regression score : "+str(score))
    plt.legend(loc='best')
    plt.show()
    
    clausesArray=np.dot(X, np.array([0, 1]))
    clausesArray=clausesArray.reshape(Y.size, -1)
    regClause=LinearRegression().fit(clausesArray, Y)
    y_predicted = regClause.intercept_ + regClause.coef_*clausesArray
    plt.figure(12)
    plt.scatter(clausesArray, Y, label="Clauses")
    plt.plot(clausesArray, y_predicted, 'k')
    plt.xlabel("Clauses")
    plt.ylabel("Increment optimisé (decay fixé)")
    plt.legend(loc='best')
    plt.show()
    
    print(score)
    #print(coef)
    #print(intercept)
    
    return score, coef, intercept, regVar


def getBestExecutionTimeFixeddecayFixedincrmt(data, stepsCount, indexIncrmt, indexDecay) :
    minExec = getExecutionTime(data, 0, 0, 0)
    ibest=0
    for i in range(stepsCount) :
        execTime = getExecutionTime(data, i, indexDecay, indexIncrmt)
        if execTime<minExec :
            minExec=execTime
            ibest=i
    
    return (minExec, getStepCount(data, ibest), ibest)


def RegressionStepsFixeddecayFixedincrmt(filesArray, indexDecay, indexIncrmt) :
    #initialise lists
    Xlist = []
    Ylist = []
    for fileRead in filesArray :
        fichier=open("lemmas_forgetting_tests/dataMap_bddTests/"+fileRead, "r")
        parsing = parseFile(fichier)
        nbVars = parsing[1]
        nbClauses = parsing[2]
        if nbVars<250 :
            Xlist.append([nbVars, nbClauses])
    
            stepsCount=parsing[5]
            stepsIncr=parsing[6]
            data=parsing[8]
            
            Ylist.append(getBestExecutionTimeFixeddecayFixedincrmt(data, stepsCount, indexIncrmt, indexDecay)[1])
            
        fichier.close()
        
    X=np.array(Xlist)
    Y=np.array(Ylist)    
    
    VarArray = np.dot(X, np.array([1, 0]))
    VarArray=VarArray.reshape(Y.size, -1)
    regVar= LinearRegression().fit(VarArray, Y)
    score = regVar.score(VarArray, Y)
    coef = regVar.coef_
    intercept = regVar.intercept_
    y_predicted = intercept + coef*VarArray
    
    plt.figure(13)
    plt.scatter(VarArray, Y, label="Variables")
    plt.plot(VarArray, y_predicted, 'k-')
    plt.xlabel("Variables")
    plt.ylabel("Increment optimisé (decay fixé)")
    plt.text(np.amax(VarArray)/2, 1, "Regression score : "+str(score))
    plt.legend(loc='best')
    plt.show()
    
    clausesArray=np.dot(X, np.array([0, 1]))
    clausesArray=clausesArray.reshape(Y.size, -1)
    regClause=LinearRegression().fit(clausesArray, Y)
    y_predicted = regClause.intercept_ + regClause.coef_*clausesArray
    plt.figure(14)
    plt.scatter(clausesArray, Y, label="Clauses")
    plt.plot(clausesArray, y_predicted, 'k')
    plt.xlabel("Clauses")
    plt.ylabel("Increment optimisé (decay fixé)")
    plt.legend(loc='best')
    plt.show()
    
    print(score)
    #print(coef)
    #print(intercept)
    
    return score, coef, intercept, regVar

def getBestExecutionTimeFixeddecayFixedincrmt(data, stepsCount, indexIncrmt, indexDecay) :
    minExec = getExecutionTime(data, 0, 0, 0)
    ibest=0
    for i in range(stepsCount) :
        execTime = getExecutionTime(data, i, indexDecay, indexIncrmt)
        if execTime<minExec :
            minExec=execTime
            ibest=i
    
    return (minExec, getStepCount(data, ibest), ibest)
    

def Speedup3(filesArray):
    #initialise lists
    Xlist = []
    Ylist = []
    lemmasdecay=1.01
    lemmasstepReg = RegressionSteps(filesArray)[-1]
    lemmasdecayReg = RegressionDecay(filesArray)[-1]
    lemmasincrmtReg = RegressionIncrementFixeddecay(filesArray, 0)[-1]
    nbFilesCount=0
    for fileRead in filesArray :
        nbFilesCount+=1
        fichier=open("lemmas_forgetting_tests/dataMap_bddTests/"+fileRead, "r")
        parsing = parseFile(fichier)
        nbVars = parsing[1]
        nbClauses=parsing[2]
        Xlist.append(nbVars)
        
        averageKeepLemmas=parsing[3]
        stepsCount=parsing[5]
        stepsIncr=parsing[6]
        stepsDecay=parsing[7]
        data=parsing[8]
        
        lemmasstep=min(lemmasstepReg.predict(nbVars)[0], nbClauses/2)
        lemmasincrmt=lemmasincrmtReg.predict(nbVars)[0]
       
        fileExec = fileRead.split("dataMap")[1].split(".txt")[0]
        print("Processing file "+fileExec+" : "+str(nbFilesCount)+" over "+str(len(filesArray)))
        x = subprocess.Popen(["./main.native", "-lemmasstep", str(int(max(1, lemmasstep))), "-lemmasincrmt", str(lemmasincrmt), "-lemmasdecay", str(lemmasdecay),  "unsat_tests/"+fileExec], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        result = x.stdout.read().decode("utf-8")
        ex = parse(result, 1)
        time = float(ex[0].split(" ")[0])
        
        Ylist.append((averageKeepLemmas-time)/averageKeepLemmas)
        
        fichier.close()
    
    X=np.array(Xlist)
    Y=np.array(Ylist)
    
    plt.figure(15)
    plt.scatter(X, Y)
    plt.xlabel("Number of Variables")
    plt.ylabel("Speedup")
    plt.show()

def Speedup4(filesArray) :
    #initialise lists
    Xlist = []
    Ylist = []

    lemmasincrmt=1.0067651012568015
    lemmasdecay=1.01
    lemmasstepReg = RegressionStepsFixeddecayFixedincrmt(filesArray, 0, 1)[-1]
    lemmasdecayReg = RegressionDecay(filesArray)[-1]
    lemmasincrmtReg = RegressionIncrementFixeddecay(filesArray, 0)[-1]
    nbFilesCount=0
    for fileRead in filesArray :
        nbFilesCount+=1
        fichier=open("lemmas_forgetting_tests/dataMap_bddTests/"+fileRead, "r")
        parsing = parseFile(fichier)
        nbVars = parsing[1]
        nbClauses=parsing[2]
        Xlist.append(nbVars)
        
        averageKeepLemmas=parsing[3]
        stepsCount=parsing[5]
        stepsIncr=parsing[6]
        stepsDecay=parsing[7]
        data=parsing[8]
        
        lemmasstep=min(lemmasstepReg.predict(nbVars)[0], nbClauses/2)
       
        fileExec = fileRead.split("dataMap")[1].split(".txt")[0]
        print("Processing file "+fileExec+" : "+str(nbFilesCount)+" over "+str(len(filesArray)))
        x = subprocess.Popen(["./main.native", "-lemmasstep", str(int(max(1, lemmasstep))), "-lemmasincrmt", str(lemmasincrmt), "-lemmasdecay", str(lemmasdecay),  "unsat_tests/"+fileExec], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        result = x.stdout.read().decode("utf-8")
        ex = parse(result, 1)
        time = float(ex[0].split(" ")[0])
        
        Ylist.append((averageKeepLemmas-time)/averageKeepLemmas)
        
        fichier.close()
    
    X=np.array(Xlist)
    Y=np.array(Ylist)
    
    plt.figure(16)
    plt.scatter(X, Y)
    plt.xlabel("Number of Variables")
    plt.ylabel("Speedup")
    plt.show()

def main2(fileRead) :
    #The file to be read
    fichier = open("lemmas_forgetting_tests/"+fileRead, "r")
    
    #parsing contains all the informations needed
    parsing = parseFile(fichier)
    
    nameFile=parsing[0]
    nbVars=parsing[1]
    nbClauses=parsing[2]
    averageKeepLemmas=parsing[3]
    nbLemmas=parsing[4]
    stepsCount=parsing[5]
    stepsIncr=parsing[6]
    stepsDecay=parsing[7]
    data=parsing[8]
    
    fichier.close()


#fileRead = "dataMap_multitests/dataMapbf0432-007.cnf"
#main(fileRead)

def main(filesArray) :
    
    RegressionSteps(filesArray)
    RegressionDecay(filesArray)
    RegressionIncrement(filesArray)
    Speedup1(filesArray)



filesArray=["dataMap_bddTests/dataMapbf0432-007.cnf.txt", "dataMap_bddTests/dataMapbf1355-075.cnf.txt", "dataMap_bddTests/dataMapbf1355-638.cnf.txt", "dataMap_bddTests/dataMapbf2670-001.cnf.txt"]

x = subprocess.Popen(["ls", "lemmas_forgetting_tests/dataMap_bddTests"] ,stdout=subprocess.PIPE, stderr=subprocess.PIPE)
filesArray=x.stdout.read().decode("utf-8").split("\n")
filesArray.pop() #to remove the last element which is " "