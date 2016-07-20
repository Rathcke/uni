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

def leastSquares(npArr, d):
    x = npArr[:, 0]
    y = npArr[:, 1]
    A = np.ones(x.shape[0])
    for i in range(1, d+1):
        A = np.c_[x**i, A]
    w = np.dot(np.dot(inv(np.dot(np.transpose(A), A)), np.transpose(A)), y)
    return w
