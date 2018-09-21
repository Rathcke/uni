from math import floor, sqrt
import numbers

def addExponent(A, key):
  n = len(A)
  for i in range(n):
    a,b = A[i]
    if key == a:
      A[i] = (a, b + 1)
      return
  A.append((key, 1))

def isPrime(n):
  if n == 1:
    return False
  if n < 4:
    return True
  if n % 2 == 0:
    return False
  if n < 9:
    return True
  if n % 3 == 0:
    return False
  end = floor(sqrt(n))
  f = 5
  while f <= end:
    if n % f == 0:
      return False
    if n % (f + 2) == 0:
      return False
    f += 6
  return True

def factorize(n):
  # TRIAL DIVISION
  # assumes n > 1
  factors = [] # list of tuples (prime, exponent)
  while True:
    #print("n = {}".format(n))
    if isPrime(n):
      addExponent(factors, n)
      return factors
    if n % 2 == 0:
      n = n // 2
      #print("dividing by {}".format(2))
      addExponent(factors, 2)
      continue
    end = int(floor(sqrt(n)))
    for i in range(3, end + 1, 2):
      if n % i == 0 and isPrime(i):
        n = n // i
        #print("dividing by {}".format(i))
        addExponent(factors, i)
        break
  return factors

if __name__ == '__main__':
  #myNumbers = [5,10,15,20,200,615,2387462222,24918723678509309615,23238616604259962647,29497513910652490397]
  myNumbers = [5,10,15,20,200,615,2387462222,24918723678509309615,23238616604259962647]
  for number in myNumbers:
    print("trial division: {} = {}".format(number, factorize(number)))
