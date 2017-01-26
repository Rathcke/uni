require 'benchmark'
require 'openssl'

def millerrabin(n,k)
    if n < 5
        return "n need to be greater than 4";
    end
    if n % 2 == 0
        return "composite";
    end
    m = n-1;
    r = 0;
    while (m % 2 == 0)
        r += 1;
        m = m/2;
    end
    for i in 1..k
        a = rand(2..n-2);
        b = a.to_bn.mod_exp(m.to_bn, n.to_bn);                      # b = a^m % n
        if (b == 1 or b == n-1)
            next;
        end
        for j in (1..(r-1))
            b = b.to_bn.mod_exp(2,n.to_bn);                   # b = b^2 % n
            if (b == 1)
              return "composite";
            end
            if (b == n-1)
                break;
            end
        end
        if (b != n-1)
            return "composite";
        end
    end
    return "probably prime";
end

numbers = [5,9,9*(2**7)+1,9*(2**9)+1,9*(2**33)+1,9*(2**31)+1,9*(2**134)+1,9*(2**132)+1,9*(2**366)+1,9*(2**368)+1,9*(2**782)+1,9*(2**780)+1]

puts "****MILLER-RABIN*****"
for i in 0..(numbers.length-1)
    puts Benchmark.measure {
        puts millerrabin(numbers[i],64);
    }
end


