from linear_regr import *
from overfitting import *

## Tests for 1.2
arr = loadData('DanWood.dt')
params = linReg(arr)
print "Parameters:"
print params
print "\n"
print "Mean Squared Error:"
print meanSqrdError(arr, params)
print "\n"

## Plot for 1.3
plt.plot(arr[:, 0], arr[:, 1], 'ro', arr[:, 0], arr[:,0]*params[0]+params[1], '--k')
plt.show()

#### Plot for question 3.2 ####
B = makeHistogram(1000, 10, 10000)

P.figure()
n, bins, patches = P.hist(B, 11, normed=1, histtype='bar',
    color=['red', 'blue', 'green'],
    label=['Mean from first coin', 'Mean from random coin', 'Minimum mean coin'])
P.legend()
P.show()

#### Plot for question 3.3 ####
x = [0.0, 0.1, 0.2, 0.3, 0.4, 0.5]
for j in range(3):
    y = []
    for i in range(len(x)):
        y.append(probUnder(B[:, j], x[i]))
    l1, = plt.plot(x, y)
    plt.legend([l1], ['Empirical Probability'])
    plt.show()

### Plot for question 3.4
x = [0.0, 0.1, 0.2, 0.3, 0.4, 0.5]
yVals = []
for elem in x:
    yVals.append(np.exp(-2*10*(0.5-elem)**2))

for j in range(3):
    y = []
    for i in range(len(x)):
        y.append(probUnder(B[:, j], x[i]))
    l1, = plt.plot(x, yVals)
    l2, = plt.plot(x, y)
    plt.legend([l1, l2], ['Bound from Hoeffding Inequality', 'Empirical Probability'])
    plt.show()
