from logistic_regr import *
from least_squares import *

### TESTING logistic_regr
a = loadData('IrisTrainML.dt')
b = loadData('IrisTestML.dt')
a = a[np.all(a != 2, axis=1)]
b = b[np.all(b != 2, axis=1)]
a[a == 0] = -1
b[b == 0] = -1

# Finding the w-vector
w = logReg(a, 10, 0.1)
print "Parameters for the model:"
print w
print "\n"

# Training error
x = a[:, [0,1]]
y = a[:, 2]
k = 0.0
for i in range(a.shape[0]):
    val = np.exp(np.dot(w, x[i]))/(1+np.exp(np.dot(w, x[i])))
    if (val < 0.5 and y[i] == 1):
        k += 1
    if (val >= 0.5 and y[i] == -1):
        k += 1

print "Error on training set with found parameters:"
print k/a.shape[0]
print "\n"

# Test error
x = b[:, [0,1]]
y = b[:, 2]
k = 0.0
for i in range(b.shape[0]):
    val = np.exp(np.dot(w, x[i]))/(1+np.exp(np.dot(w, x[i])))
    if (val < 0.5 and y[i] == 1):
        k += 1
    if (val >= 0.5 and y[i] == -1):
        k += 1

print "Error on test set with found parameters:"
print k/b.shape[0]

### TESTING least_squares
a = loadData('coords.dt')

# Least squares with d = 2
print "\n"
print "Parameters from least squares with d=2:"
w = leastSquares(a, 2)
print w

# Plot to compare
xVals = np.arange(0.0,10.3,0.1)
plt.plot(a[:, 0], a[:, 1], 'ro', xVals, w[0]*xVals**2+w[1]*xVals+w[2], '--k')
plt.xlabel('Distance')
plt.ylabel('Height')
plt.show()
