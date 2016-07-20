import numpy as np
import pylab as P
import matplotlib.pyplot as plt

def flipCoins(n, m):
    A = np.random.randint(2, size=(n, m))
    return np.array(A)

def meanRows(A):
    return A.mean(axis=1)

def makeHistogram(n, m, reps):
    B = []
    for i in range(reps):
        A = meanRows(flipCoins(n, m))
        B.append([A[0], A[np.random.randint(1, n)], min(A)])
    return np.array(B)

def probUnder(A, x):
    return (A <= x).sum()/10000.0
