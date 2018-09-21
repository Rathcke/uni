require './factorization.rb'
require 'openssl'
require 'benchmark'

def pocklingtonTest(n)
  if n % 2 == 0 && n != 2
    return false
  end
  a = chooseABeta(n)
  primeFactorization = factorize(a)
  primeFactors = getPrimeFactors(primeFactorization)
  for i in 0..primeFactors.length-1
    x = primeFactors[i]
    if not conditionsHold(x, n)
      return "composite"
    end
  end
  return "prime"
end

def conditionsHold(p, n)
  a = 2
  while a < n - 1
    if (firstCondition(a, n) && secondCondition(a, p, n))
      return true
    end
    a += 1
  end
  return false
end

def firstCondition(a, n)
  return a.to_bn.mod_exp(n-1, n).to_i == 1
end

def secondCondition(a, p, n)
    a = ((a.to_bn ** ((n-1)/p) - 1) % n).to_i
  return n.gcd(a) == 1
end

def chooseABeta(n)
  i = Math.sqrt(n).ceil
  while i <= n - 1
    if ((n - 1) % i) == 0 && (i.gcd((n - 1) / i) == 1)
      return i
    end
    i += 1
  end
end

def getPrimeFactors(liste)
  rlist = []
  for x in liste
    rlist.push(x[0])
  end
  return rlist
end

numbers = [5,9,9*(2**7)+1,9*(2**9)+1,9*(2**33)+1,9*(2**31)+1,9*(2**134)+1,9*(2**132)+1,9*(2**366)+1,9*(2**368)+1,9*(2**782)+1,9*(2**780)+1]

puts "*****POCKLINGTON*****"
for i in 0..(numbers.length-1)
   puts Benchmark.measure {
      puts pocklingtonTest(numbers[i]);
    }
end