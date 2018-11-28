import os
import subprocess
import numpy as np
import matplotlib.pyplot as plt

def parseLine(line) :
    result = line.split(" ")
    result[1]=float(result[1])
    result[2]=int(result[2])
    result[3]=float(result[3])
    result[4]=float(result[4])
    return result
    
def parseBlock(block):
    table = block.split("\n")
    result = []
    result.append(float(table[0].split("=")[-1]))
    for i in range(1, len(table)-1) :
        result.append(parseLine(table[i]))
    return result   
    
def parseFile(file):
    line = file.readline();
    nbFiles = int(line.split(" ")[-1])
    line = file.readline();
    steps = int(line.split(" ")[-1])
    line = file.read().split("===========================\n");
    result=[]
    for i in range(1, len(line)) :
        result.append(parseBlock(line[i]))
    return (nbFiles, steps, result)
        
    
fileRead = "data3.txt"
fichier = open("lemmas_forgetting_tests/"+fileRead, "r")

parsing = parseFile(fichier)
nbFiles=parsing[0]
steps=parsing[1]
data=parsing[2]
#print(len(data))

Xlist= []
for i in range(len(data)-1):
    Xlist.append(data[i][0])

Xaxis = np.array(Xlist)

listOfYAxislists = []

for i in range(nbFiles) :
    listOfYAxislists.append([])

for i in range(steps) :
    for j in range(nbFiles) :
        listOfYAxislists[j].append(data[i][j+1][1])

listOfYAxis=[]

for i in range(len(listOfYAxislists)) :
    listOfYAxis.append(np.array(listOfYAxislists[i]))

plt.plot(Xaxis, listOfYAxis[1])

plt.show()

fichier.close()