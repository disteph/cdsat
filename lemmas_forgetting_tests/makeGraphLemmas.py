# Making the graph for MaxCoutn and Increment step variation
# Need to be launched from the directory cdsat.

import os
import subprocess
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import math

#Parse a line from dataMap texts (with only one file like dubois100.cnf)
def parseLine(line) :
    result = line.split(" ")
    if (result[0]=="XXX") :
        
        result[1]=40.0
        return result
    else :
        result[1]=float(result[1])
        result[2]=int(result[2])
        result[3]=float(result[3])
        result[4]=float(result[4])
        return result
    
#Parse a block, that is the text between two "==========="
# That is : first, the value of Increment and second, one line whose sense is given by the line "File name / Execution time / Number of decisions / Decisions per second / Options per decision"
def parseBlock(block):
    table = block.split("\n")
    result = []
    result.append(float(table[0].split("=")[-1]))
    for i in range(1, len(table)-1) :
        result.append(parseLine(table[i]))
    return result   
    
#Parse the whole file
#The file must look like dataMap3.txt, that is :
# 1) Execution time with the tag -keeplemmas
# 2) Number of file (here : one)
# 3) Number of arguments for the tag -lemmasstep
# 4) Number of arguments for the tag -lemmasincrmt
# Then, all the blocks where the parameters MaxCount and Increment appear. 
def parseFile(file):
    line = file.readline();
    averageKeepLemmas = float(line.split(" ")[-1].split("\n")[0])
    line = file.readline();
    nbFiles = int(line.split(" ")[-1])
    line = file.readline();
    stepsCount = int(line.split(" ")[-1])
    line = file.readline();
    stepsIncr = int(line.split(" ")[-1])
    
    text = file.read().split("***\n");
    data = []
    for j in range(1, stepsCount+1):
        
        line = text[j].split("===========================\n");
        result=[math.ceil(float(line[0].split("=")[-1].split("\n")[0]))]
        for i in range(1, len(line)) :
            result.append(parseBlock(line[i]))
        data.append(result)
    return (nbFiles, stepsCount, stepsIncr, data, averageKeepLemmas)
        

#The file to be read
fileRead = "dataMap5.txt"
fichier = open("lemmas_forgetting_tests/"+fileRead, "r")

parsing = parseFile(fichier)

# We get the data here. The result of the parser gives [nbFiles, steps, result]
# result has the form [[Max_Count1, block_1], ..., [MaxCount_n, block_n]]
# block_p has the form [[nbIncr1, name, time, nbDec, nbDecPerSec, optPerDec], ..., [nbIncrm, name, time, nbDec, nbDecPerSec, optPerDec]]


nbFiles=parsing[0]
stepsCount=parsing[1]
stepsIncr=parsing[2]
data=parsing[3]
averageKeepLemmas = parsing[4]

XlistCount= []
for i in range(len(data)):
    XlistCount.append(data[i][0])

XaxisCount = np.array(XlistCount)

XlistIncr=[]
for i in range(1, len(data[0])):
    XlistIncr.append(data[0][i][0])    

XaxisIncr = np.array(XlistIncr)

listOfYAxislists = []

for i in range(stepsCount) :
    listOfYAxislists.append([])



for i in range(stepsCount) :
    for j in range(stepsIncr) :
        listOfYAxislists[i].append(data[i][j+1][1][1])

listOfYAxis=[]

for i in range(len(listOfYAxislists)) :
    listOfYAxis.append(np.array(listOfYAxislists[i]))


for number in range(len(listOfYAxis)):
    plt.plot(XaxisIncr, listOfYAxis[number], label=str(XaxisCount[number]))


plt.plot([XaxisIncr[0], XaxisIncr[-1]], [averageKeepLemmas, averageKeepLemmas], 'r--', lw=2, label="keepLemmas")
plt.xlabel("Incrementation")
plt.ylabel("Performance (time)")
plt.legend(loc='best')
plt.show()


fichier.close()