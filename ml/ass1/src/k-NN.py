import math

def loadData(fname):
    with open(fname, 'r') as f:
        lines = f.read().splitlines()
        arr = []
        for l in lines:
            arr.append(map(float, l.split(" ")))
        f.close
    return arr

def knn(trainArr, testArr, distMetric, k):
    results = []
    for i in testArr:
        kBest = []
        distances = []
        for j in trainArr:
            distances.append(distMetric(i, j))
        maxIndex = distances.index(max(distances))
        for h in range(k):
            minIndex = distances.index(min(distances))
            kBest.append(trainArr[minIndex])
            distances[minIndex] = distances[maxIndex]
        results.append(kBest)
    classifications = getClassification(results)
    return classifications

def euclidianPlaneDist(a, b):
    return math.sqrt((a[0]-b[0])**2+(a[1]-b[1])**2)

def getClassification(arr):
    classifications = []
    for a in arr:
        classifiers = []
        for b in a:
            classifiers.append(b[2])
        classifications.append(max(set(classifiers), key=classifiers.count))
    return classifications

### 5-fold cross-validation ###
def errorOf(arr, tArr):
    k = 0
    for i in range(len(tArr)):
        if tArr[i][2] != arr[i]:
            k += 1.0
    return k/len(tArr)

def knn_best(trainArr, k):
    length = len(trainArr)
    indices = []
    kb = 0
    kBest = 0
    for i in range(0, k):
        indices.append(i*length/k)
    indices.append(length)
    for i in range(1, 26):
        res = 0.0
        for j in range(k):
            res += errorOf(knn(trainArr[0:indices[j]] + trainArr[indices[j+1]:indices[-1]],
                trainArr[indices[j]:indices[j+1]], euclidianPlaneDist, i),
                trainArr[indices[j]:indices[j+1]])
        if i == 1:
            kb = i
            kBest = res
        if res < kBest:
            kBest = res
            kb = i
    return (kb, kBest/k)

### Data Normalization ###
def calcMeanAndVar(train):
    length = len(train[0])
    meanVec = [0] * (length-1)
    varVec = [0] * (length-1)
    for a in train:
        for i in range(length-1):
            meanVec[i] += a[i]
    for i in range(length-1):
        meanVec[i] = meanVec[i] / len(train)
    for a in train:
        for i in range(length-1):
            varVec[i] += (a[i]-meanVec[i])**2
    for i in range(length-1):
        varVec[i] = varVec[i] / len(train)
    return [meanVec, varVec]

def fNorm(set1, meanAndVarFromSet):
    normSet = []
    meanVar = calcMeanAndVar(meanAndVarFromSet)
    for j in range(len(set1)):
        normVec = set1[j]
        for i in range(len(set1[0])-1):
            normVec[i] = (normVec[i]-meanVar[0][i])/math.sqrt(meanVar[1][i])
        normSet.append(normVec)
    return normSet



#####################################
########### TEST RESULTS ############
#####################################

trainT = loadData('IrisTrainML.dt')
testT  = loadData('IrisTestML.dt')

#### knn tests ####

# K = 1
print "knn-test with k = 1:"
b = knn(trainT, testT, euclidianPlaneDist, 1)
print b
print errorOf(b, testT)
print "\n"

# K = 3
print "knn-test with k = 3:"
b = knn(trainT, testT, euclidianPlaneDist, 3)
print b
print errorOf(b, testT)
print "\n"

# K = 5
print "knn-test with k = 5:"
b = knn(trainT, testT, euclidianPlaneDist, 5)
print b
print errorOf(b, testT)
print "\n"

### knn_best tests ####

# Hyperparameter selection
print "Hyperparameter selection:"
print knn_best(trainT, 5)
print "\n"

# K = 4
print "Running knn with found parameter, k = 4:"
b = knn(trainT, testT, euclidianPlaneDist, 4)
print b
print errorOf(b, testT)
print "\n"

### fNorm tests ####
print "Calculating mean and variance"
print calcMeanAndVar(trainT)
print "\n"

print "Mean and variance when the training set is normalized:"
print calcMeanAndVar(fNorm(trainT, trainT))
print "\n"

print "Hyperparameter selection from normalized data:"
print knn_best(fNorm(trainT, trainT), 5)
print "\n"

# K = 1, Normalized data
print "Running knn with found parameter, k=1, on normalized data:"
b = knn(fNorm(loadData('IrisTrainML.dt'), loadData('IrisTrainML.dt')), fNorm(loadData('IrisTestML.dt'), loadData('IrisTrainML.dt')), euclidianPlaneDist, 1)
print b
print errorOf(b, testT)
print "\n"

# K = 3, Normalized data
print "Test with k=3 for comparison:"
b = knn(fNorm(loadData('IrisTrainML.dt'), loadData('IrisTrainML.dt')), fNorm(loadData('IrisTestML.dt'), loadData('IrisTrainML.dt')), euclidianPlaneDist, 3)
print b
print errorOf(b, testT)
print "\n"

# K = 5, Normalized data
print "Test with k=5 for comparison:"
b = knn(fNorm(loadData('IrisTrainML.dt'), loadData('IrisTrainML.dt')), fNorm(loadData('IrisTestML.dt'), loadData('IrisTrainML.dt')), euclidianPlaneDist, 5)
print b
print errorOf(b, testT)

####################################
