import numpy as np

def loadData(fname):
    data = []
    with open(fname, 'r') as f:
        for l in f:
            data.append(l.split(" "))
        f.close
    return np.array(data, float)

def h(x, deriv=False):
    if(deriv==True):
        return 1/(1+abs(x))**2
    return x/(1+abs(x))

####################################

def nn(x, y, w1, w2, flag):

    #Forward Propagation
    x0 = np.array([[1], [x]], float)
    s1 = np.dot(w1.T, x0)
    x1 = np.vstack((np.array([[1]], float),h(s1)))
    s2 = np.dot(w2.T, x1)
    x2 = h(s2)

    if flag:
        return (x2 -y)**2

    d2 = 2 * (x2 - y) * h(s2, deriv=True)

    d1 = w2.dot(d2) * h(s1, deriv=True)

    w1 += x1.T.dot(d1) / 25.0
    w2 += x2.T.dot(d2) / 25.0

    return (w1, w2)

####################################

np.random.seed(1)
trainT = loadData('sincTrain25.dt')
trainX = np.delete(trainT, 1, 1)
trainY = np.delete(trainT, 0, 1)

w1 = 2*np.random.rand(2, 1) - 1
w2 = 2*np.random.rand(2, 1) - 1

for (x, y) in trainT:
    (w1, w2) = nn(x, y, w1, w2, False)

err = 0.0
for (x, y) in trainT:
    err += nn(x, y, w1, w2, True)
print err/25.0

###################################
