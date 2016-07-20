import numpy as np
from numpy.linalg import eig
import matplotlib.pyplot as plt

def loadData(fname):
    data = []
    with open(fname, 'r') as f:
        for l in f:
            data.append(l.split(","))
        f.close
    return np.array(data, float)

data = loadData('keystrokesTrainTwoClass.csv')
data = data[:, 0:21]
meanV = np.mean(data, axis=0)
covM = np.cov(data, rowvar=0)
eigVal, eigVec = np.linalg.eig(covM)
eigP = []
for i in range(len(eigVal)):
    eigP.append((np.abs(eigVal[i]), eigVec[:,i]))
eigP.sort()
eigP.reverse()
matW = np.hstack((eigP[0][1].reshape(21,1), eigP[1][1].reshape(21,1)))
newData = np.dot(np.transpose(matW), np.transpose(data))

### PLOT eigenspectrum ###
eigVal.sort()
plt.plot(np.arange(1,22), eigVal[::-1], 'k')
plt.yscale('log')
plt.title('Eigenspectrum')
plt.show()

### PLOT scatter ###
print newData
plt.plot(newData[0, 0:320], newData[1, 0:320], 'ro', label='class 0')
plt.plot(newData[0, 320:640], newData[1, 320:640], 'bo', label='class 1')
plt.xlabel('x-values')
plt.ylabel('y-values')
plt.legend()
plt.title('Result from PCA')
plt.show()
