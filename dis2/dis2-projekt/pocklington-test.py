import math
import fractions
import operator

def pocklingtonTest(N):
    A = chooseAbeta(N);
    PrimeFactorization = Christoffer(N)
    for x in PrimeFactorization:
        if(not conditionsHold(x, N)):
            return false;
    return true;

def conditionsHold(p, N):
    a = 2;
    while(a < N-1):
        if(firstCondition(a, N) and secondCondition(a, p, N)):
            return true;
        a += 1;
    return false; 

def firstCondition(a, N):
    return (a**(N-1))%N == 1;

def secondCondition(a, p, N):
    return fractions.gcd(a**((N-1)/p), N) == 1;

def chooseAbeta(N):
    for A in range(math.sqrt(N),N-1):
        if ((N-1)%A == 0 and fractions.gcd(A, N-A)==1):
            return A;

def chooseA(PrimeFactorization, N):
    A = 0;
    B = 0;
    i = 1;
    while True:
        Alist = calcTuple(PrimeFactorization[i:]);
        Blist = calcTuple(PrimeFactorization[:i]);
        A = reduce(operator.mul, Alist, 1);
        B = reduce(operator.mul, Blist, 1);
        if(fractions.gcd(A, B) == 1):
            break
        i += 1;
    return A;

def calcTuple(liste):
    rlist = [];
    for x in liste:
        rlist.append(x[0]**x[1]);
    return rlist;

print pocklingtonTest([(2, 1),(3, 1)], 7);





