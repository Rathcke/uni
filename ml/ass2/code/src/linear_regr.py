import numpy as np
from numpy.linalg import inv
import matplotlib.pyplot as plt

def loadData(fname):
    data = []
    with open(fname, 'r') as f:
        for l in f:
            data.append(l.split(" "))
        f.close
    return np.array(data, float)

def linReg(npArr):
    y = npArr[:,1]
    X = np.column_stack((npArr[:,0], np.ones((npArr.shape[0], 1))))
    A = np.dot(np.dot(inv(np.dot(np.transpose(X), X)), np.transpose(X)), y)
    return A

def meanSqrdError(npArr, params):
    err = 0
    yCalc = npArr[:,0]*params[0]+params[1]
    for i in range(npArr.shape[0]):
        err += (npArr[i][1] - yCalc[i])**2
    return err/npArr.shape[0]
