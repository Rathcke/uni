import numpy as np
import matplotlib.pyplot as plt

def loadData(fname):
    data = []
    with open(fname, 'r') as f:
        for l in f:
            data.append(l.split(" "))
        f.close
    return np.array(data, float)

data = loadData('localTest')
data2 = loadData('nonLocalTest')

print data2

plt.plot(data[:,0], data[:,1], color='blue', label='Same address space')
plt.plot(data2[:,0], data2[:,1], color='red', label='Different address space')
plt.yscale('log')
plt.xlabel('Number of threads')
plt.ylabel('Throughput per nanosecond')
plt.legend()
plt.title('Local test: Threads vs Throughput')
plt.show()

plt.plot(data[:,0], data[:,2], color='blue', label='Same address space')
plt.plot(data2[:,0], data2[:,2], color='red', label='Different address space')
plt.yscale('log')
plt.xlabel('Number of threads')
plt.ylabel('Latency in nanoseconds')
plt.legend()
plt.title('Local test: Threads vs Latency')
plt.show()
