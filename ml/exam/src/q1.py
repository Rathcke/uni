import numpy as np
from numpy.linalg import *
import matplotlib.pyplot as plt
import pylab as P
from sklearn.preprocessing import normalize

### Load data ###
def loadData(fname):
    data = []
    with open(fname, 'r') as f:
        for l in f:
            data.append(l.split(","))
        f.close
    return np.array(data, float)

data = loadData('ML2015TrafficSignsTrain.csv')
classes = data[:, 1568]
data = data[:, 0:1568]

### Normalize data ###
meanVec = np.mean(data, axis=0)
varVec  = np.var(data, axis=0)
for i in range(len(data)):
    data[i] = data[i] - meanVec
    data[i] = data[i] / np.sqrt(varVec)

### Histogram ###
def makeFreqHistogram(size, classes):
    P.figure()
    n, bins, patches = P.hist(classes, size, normed=1, histtype='bar',
        color=['red'],
        label=['Traffic sign frequency'])
    P.legend()
    P.show()
    return

makeFreqHistogram(43, classes)

### PCA ###
def pca(data, classes):
    data = data[:, 0:1568]
    meanV = np.mean(data, axis=0)
    covM = np.cov(data, rowvar=0)
    eigVal, eigVec = np.linalg.eig(covM)
    eigVal = np.real(eigVal)
    eigVec = np.real(eigVec)
    eigP = []
    for i in range(len(eigVal)):
        eigP.append((np.abs(eigVal[i]), eigVec[:,i]))
    matW = np.hstack((eigP[0][1].reshape(1568, 1), eigP[1][1].reshape(1568,1)))
    newData = np.dot(np.transpose(matW), np.transpose(data))
    return (eigVal, newData)

### Eigenspectrum plot ###
(eigVal, newData) = pca(data, classes)
eigVal.sort()
plt.yscale('log')
plt.plot(np.arange(1,1569), eigVal[::-1], 'k')
plt.title('Eigenspectrum')
plt.show()

### Number of principal components to explain 90% of variance ###
var90 = np.sum(eigVal)*0.9
b = 0.0
i = 0
for a in eigVal[::-1]:
    i += 1
    b += a
    if (b > var90):
        print "Number of principal components to explain 90% of the variance:"
        print i
        break

### Scatter plot ###
for i in range(1275):
    c = classes[i]
    if (c == 0 or c == 1 or c == 2 or c == 3 or c == 4 or c == 5 or c == 6 or
            c == 7 or c == 8 or c == 9 or c == 10 or c == 15 or c == 16 or
            c == 17 or c == 32 or c == 33 or c == 34 or c == 35 or c == 36 or
            c == 37 or c == 38 or c == 39 or c == 40 or c == 41 or c == 42):
        plt.plot(newData[0, i], newData[1, i], 'ro')
    elif (c == 11 or c == 18 or c == 19 or c == 20 or c == 21 or c == 22 or
                c == 23 or c == 24 or c == 25 or c == 26 or c == 27 or
                c == 28 or c == 29 or c == 30 or c == 31):
        plt.plot(newData[0, i], newData[1, i], 'b^')
    elif (c == 12):
        plt.plot(newData[0, i], newData[1, i], 'gD')
    elif (c == 13):
        plt.plot(newData[0, i], newData[1, i], 'kv')
    else:
        plt.plot(newData[0, i], newData[1, i], 'y8')
plt.xlabel('x-values')
plt.ylabel('y-values')
plt.title('Result from PCA')
plt.show()

### K-means Clustering ###
np.random.seed(1)
def kmeans(k, data, classes):
    entries = data.shape[0]
    centroids = [None] * k

    for i in range(k):
        rand = np.random.random_integers(0, entries-1)
        centroids[i] = data[rand]

    centroids = np.array(centroids, float)
    dists = np.zeros((entries, k))
    labels = np.zeros((entries, 1))
    oldCentroids = np.zeros(centroids.shape)

    while (np.linalg.norm(centroids - oldCentroids) > 0.0001):
        oldCentroids = np.copy(centroids)

        for i in range(k):
            for j in range(entries):
                dists[j, i] = np.linalg.norm(centroids[i] - data[j])
        for i in range(entries):
            entryDists = dists[i ,:]
            labels[i] = np.argmin(entryDists)
        for i in range(k):
            centroids[i] = np.mean(data[labels[:, 0] == i], axis=0)

    return centroids

cents = kmeans(4, data, classes)
modData = np.r_[data, cents]
modClasses = np.append(classes, [99, 99, 99, 99])

### Scatter plot with centroids ###
(eigVal, newData) = pca(modData, modClasses)

for i in range(1279):
    c = modClasses[i]
    if (c == 0 or c == 1 or c == 2 or c == 3 or c == 4 or c == 5 or c == 6 or
            c == 7 or c == 8 or c == 9 or c == 10 or c == 15 or c == 16 or
            c == 17 or c == 32 or c == 33 or c == 34 or c == 35 or c == 36 or
            c == 37 or c == 38 or c == 39 or c == 40 or c == 41 or c == 42):
        plt.plot(newData[0, i], newData[1, i], 'ro')
    elif (c == 11 or c == 18 or c == 19 or c == 20 or c == 21 or c == 22 or
                c == 23 or c == 24 or c == 25 or c == 26 or c == 27 or
                c == 28 or c == 29 or c == 30 or c == 31):
        plt.plot(newData[0, i], newData[1, i], 'b^')
    elif (c == 12):
        plt.plot(newData[0, i], newData[1, i], 'gD')
    elif (c == 13):
        plt.plot(newData[0, i], newData[1, i], 'kv')
    elif (c == 99):
        plt.plot(newData[0, i], newData[1, i], 'm*', ms=20)
    else:
        plt.plot(newData[0, i], newData[1, i], 'y8')
plt.xlabel('x-values')
plt.ylabel('y-values')
plt.title('Result from 4-means clustering')
plt.show()
