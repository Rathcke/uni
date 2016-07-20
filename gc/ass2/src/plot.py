import matplotlib.pyplot as plt
from math import log

pts =  [(100, 14.4), (200, 21.1), (300, 31.6), (400, 51.8), (500, 59.3), (600, 59.3), (700, 71.8), (800, 92.1), (900, 97.5), (1000, 114.9), (1100, 113.6), (1200, 117.6), (1300, 117.4), (1400, 154.5), (1500, 167.4), (1600, 150.1), (1700, 157.7), (1800, 179.9), (1900, 178.3), (2000, 160.2)]

pts2 = [(elem1, elem2) for elem1, elem2 in pts]

#plt.title('g(n) estimation')
#plt.xlabel('Number of vertices')
#plt.ylabel('Number of colors used by greedy algorithm')
#plt.plot(*zip(*pts2))
#plt.show()

pts3 = [(elem1, elem2/(elem1/log(elem1, 2))) for elem1, elem2 in pts]

plt.title('Estimation of f(n) using g(n) result')
plt.xlabel('Number of vertices')
plt.ylabel('f(n) value')
plt.plot(*zip(*pts3))
plt.show()
