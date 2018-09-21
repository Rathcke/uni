import math;

def prothTest(h,k):
    n = h*(2**k)+1;
    if(not (2**k > h) and not h%2 == 0):
        return 'h needs to be smaller than 2**k';
    for a in range(1,n):
        if((a**((n-1)/2))%n == (-1)%n):
            return str(n) + ' is a prime';
    return str(n) + ' is not a prime';

print(prothTest(10,12));

