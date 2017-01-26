import math
import fractions
import operator
import factorization
import decimal

def pocklingtonTest(N):
    if((N % 2) == 0):
        return False
    A = chooseAbeta(N);
    primeFactorization = factorization.factorize(A);
    primeFactors = getPrimeFactors(primeFactorization);
    for x in primeFactors:
        if(not conditionsHold(x, N)):
            return False;
    return True;

def conditionsHold(p, N):
    a = 2;
    while(a < N-1):
        if(firstCondition(a, N) and secondCondition(a, p, N)):
            return True;
        a += 1;
    return False;

def firstCondition(a, N):
    return ((a**(N-1))%N == 1);

def secondCondition(a, p, N):
    return fractions.gcd(a**((N-1)/p)-1, N) == 1;

def chooseAbeta(N):
    for A in range(int(math.ceil(math.sqrt(N))),N-1):
        if ((N-1)%A == 0 and fractions.gcd(A, (N-1)/A)==1):
            return A;

def getPrimeFactors(liste):
    rlist = [];
    for x in liste:
        rlist.append(x[0]);
    return rlist;

print(pocklingtonTest(986094332123));




