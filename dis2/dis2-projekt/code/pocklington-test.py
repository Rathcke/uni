import math
import fractions
import operator
import factorization
import gmpy2

def pocklingtonTest(N):
    if((N % 2) == 0 and N != 2):
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
    print "a is " + str(a)
    print a**(N-1)%N
    return ((a**(N-1))%N == 1);

def secondCondition(a, p, N):
    print a**((N-1)/p)-1
    return fractions.gcd(a**((N-1)/p)-1, N) == 1;

def chooseAbeta(N):
    i = gmpy2.isqrt(N)+1
    while(i <= N-1):
        if ((N-1)%i == 0 and fractions.gcd(i, (N-1)/i)==1):
            return i;
        i += 1

def getPrimeFactors(liste):
    rlist = [];
    for x in liste:
        rlist.append(x[0]);
    return rlist;

print(pocklingtonTest(615))
