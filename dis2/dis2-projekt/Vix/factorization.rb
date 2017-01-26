

def addExponent(arr, key)
  n = arr.length;
  for i in 0..n-1
    a = arr[i][0];
    b = arr[i][1];
    if key == a
      arr[i] = [a, b + 1];
      return;
    end
  end
  arr.push([key, 1]);
end

def isPrime(n)
  if n == 1
    return false;
  end
  if n < 4
    return true;
  end
  if n % 2 == 0
    return false;
  end
  if n < 9
    return true;
  end
  if n % 3 == 0
    return false;
  end
  stop = Math.sqrt(n).floor;
  f = 5;
  while f <= stop
    if n % f == 0
      return false;
    end
    if n % (f + 2) == 0
      return false;
    end
    f += 6;
  end
  return true;
end

def factorize(n)
  factors = []
  while true
    if isPrime(n)
      addExponent(factors, n)
      return factors
    end
    if n % 2 == 0
      n = n / 2
      addExponent(factors, 2)
      next
    end
    stop = Math.sqrt(n).floor
    for i in (3..stop) . step(2)
      if n % i == 0 && isPrime(i)
        n = n / i
        addExponent(factors, i)
        break
      end
    end
  end
  return factors
end

=begin
#myNumbers = [5,10,15,20,200,615,2387462222,24918723678509309615,23238616604259962647,29497513910652490397]
myNumbers = [5,10,15,20,200,615,2387462222,24918723678509309615,23238616604259962647]
for i in 0..myNumbers.length-1
  a = myNumbers[i]
  b = factorize(a)
  puts("trial division: #{a} = #{b}")
end
=end
