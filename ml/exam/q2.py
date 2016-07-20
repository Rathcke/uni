import numpy as np
from numpy.linalg import *
import matplotlib.pyplot as plt
import pylab as P
from sklearn import *

### Load data ###
def loadData(fname):
    data = []
    with open(fname, 'r') as f:
        for l in f:
            data.append(l.split(","))
        f.close
    return np.array(data, float)

data = loadData('ML2015StarsGalaxiesTrain.csv')
classes = data[:, 10]
data = data[:, 0:10]
testData = loadData('ML2015StarsGalaxiesTest.csv')
testClasses = testData[:, 10]
testData = testData[:, 0:10]

### Normalize data ###
meanVec = np.mean(data, axis=0)
varVec  = np.var(data, axis=0)
for i in range(len(data)):
    data[i] = data[i] - meanVec
    data[i] = data[i] / np.sqrt(varVec)
    testData[i] = testData[i] - meanVec
    testData[i] = testData[i] - np.sqrt(varVec)

### Binary classification using C-SVM ###
def calcMedian(data, classes):
    shape = data.shape
    minDists = [None] * shape[0]
    for i in range(shape[0]):
        for j in range(shape[0]):
            if (classes[i] == classes[j]):
                continue
            dist = np.linalg.norm(data[i]-data[j])
            if (minDists[i] == None):
                minDists[i] = dist
            if (dist < minDists[i]):
                minDists[i] = dist
    return np.median(minDists)

median = calcMedian(data, classes)
b = 10
C = np.array([b**(-3), b**(-2), b**(-1), 1, b, b**2, b**3], float)
jaak = 1/(2*median**2)
print "Gamma value suggested by the Jaakkola Heuristic:"
print jaak
gamma = jaak * C
parameters = [{'gamma': gamma, 'C': C, 'kernel': ['rbf']}]
svr = svm.SVC(kernel='rbf')
clf = grid_search.GridSearchCV(svr, parameters, cv=5)
clf.fit(data, classes)
print "Best parameters:"
print clf.best_params_
print "Success rate for training data:"
print clf.score(data, classes)
print "Success rate for test data:"
print clf.score(testData, testClasses)
