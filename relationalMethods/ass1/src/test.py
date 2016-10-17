import numpy as np

#R = np.array([[0,0,1,0,0,0,1,0],[0,0,1,1,0,0,0,0],[0,0,0,0,0,0,0,0],
#              [1,0,0,1,0,0,0,0],[0,0,0,0,0,1,0,0],[1,0,0,0,0,1,0,0],
#              [0,0,1,1,0,0,0,0],[1,0,0,0,0,1,0,1]])
#I = np.array([[1,0,0,0,0,0,0,0],[0,1,0,0,0,0,0,0],[0,0,1,0,0,0,0,0],
#              [0,0,0,1,0,0,0,0],[0,0,0,0,1,0,0,0],[0,0,0,0,0,1,0,0],
#              [0,0,0,0,0,0,1,0],[0,0,0,0,0,0,0,1]])
#print np.dot(R, S).T
#print np.dot(R.T, S.T)

R = np.array([[1,0,0,1],[0,1,0,0],[1,0,1,1],[1,0,0,0]])
S = np.array([[0,0,1,0],[0,0,0,0],[0,0,0,0],[0,0,1,0]])

print np.dot(R,R.T)

#print np.dot(R,R)
#print np.dot(np.dot(R,R),R)
#print np.dot(np.dot(np.dot(R,R),R),R)
#print np.dot(np.dot(np.dot(np.dot(R,R),R),R),R)
#print np.dot(np.dot(np.dot(np.dot(np.dot(R,R),R),R),R),R)

