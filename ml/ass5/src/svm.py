import numpy as np

def loadData(fname):
    data = []
    with open(fname, 'r') as f:
        for l in f:
            data.append(l.split(" "))
        f.close
    return np.array(data, float)

### Data Normalization ###
def calcMeanAndVar(set1):
    mean = np.mean(set1, axis=0)
    var  = np.var(set1, axis=0)
    return [mean, var]

def fNorm(set1, meanAndVarFromSet):
    normSet = []
    mean    = np.mean(meanAndVarFromSet, axis=0)
    stdDev  = np.sqrt(np.var(meanAndVarFromSet, axis=0))
    for a in set1:
        normSet.append((a-mean)/stdDev)
    return np.array(normSet, float)

#####################################
########### TEST RESULTS ############
#####################################

train = loadData('parkinsonsTrainStatML.dt')
test  = loadData('parkinsonsTestStatML.dt')
trainT = np.delete(train, 22, 1)
testT  = np.delete(test, 22, 1)

### fNorm tests ####
print "Calculating mean and variance"
print calcMeanAndVar(trainT)
print "\n"

print "Mean and variance when the training set is normalized:"
print calcMeanAndVar(fNorm(testT, trainT))
print "\n"


####################################
