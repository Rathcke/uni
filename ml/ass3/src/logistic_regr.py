import numpy as np

def loadData(fname):
    data = []
    with open(fname, 'r') as f:
        for l in f:
            data.append(l.split(" "))
        f.close
    return np.array(data, float)

def logReg(npArr, t, lRate):
    w = np.array([0,0], float)
    x = npArr[:, [0,1]]
    y = npArr[:, 2]
    for i in range(t):
        g = 0
        for j in range(npArr.shape[0]):
            g +=y[j]*x[j]/(1+np.exp(y[j]*np.dot(w, x[j])))
        g = -g/npArr.shape[0]
        v = -g
        w += lRate*v
    return w
