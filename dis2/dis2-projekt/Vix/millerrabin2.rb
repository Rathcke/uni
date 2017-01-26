require 'benchmark'
require 'openssl'
 
def millerrabin(n,k)
    if n < 5
        return "n need to be greater than 4"
    end
    if n % 2 == 0
        return "#{n} is composite"
    end
    m = n-1;
    r = 0;
    while (m % 2 == 0)
        r += 1;
        m = m/2;
    end
    for i in 1..k
        a = rand(2..n-2);
        b = a.to_bn.mod_exp(m, n)                       # b = a^m % n
        if (b == 1 or b == n-1)
            next;
        end
        for j in (1..(r-1))
            b = b.to_bn.mod_exp(2,n)                    # b = b^2 % n
            if (b == 1)
              return "#{n} is composite"
            end
            if (b == n-1)
                break;
            end
        end
        if (b != n-1)
            return "#{n} is composite";
        end
    end
    return "#{n} is probably prime";
end
 
numbers = [48112959837082048697, 54673257461630679457, 29497513910652490397, 40206835204840513073, 12764787846358441471, 71755440315342536873, 45095080578985454453, 27542476619900900873, 66405897020462343733, 36413321723440003717, 24918723678509309615, 37813311192850760755, 50608046447142653959, 32395252670977894437, 19601290870353624837, 13875069371204789371, 41875470038172930717, 23238616604259962647, 31242090723917058139, 92427220233223221283]

puts Benchmark.measure {
    for i in 0..19
       puts millerrabin(numbers[i],64)
    end
}