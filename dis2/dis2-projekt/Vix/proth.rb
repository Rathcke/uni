require 'benchmark'
require 'openssl'
require 'prime'

def proth(n,k)
	t = n-1
	e = 0
	if n % 2 == 0
		return "must be an odd number"
	end
	while t % 2 == 0
		e += 1
		t = t/2
	end
	for i in 1..k
		a = rand(1..n-1)
		b = a.to_bn.mod_exp((n-1)/2,n)
		if b == n-1
			return "prime"
		end
		if ((b != 1) && (b != n-1)) && (b.to_bn.mod_exp(2,n) == 1)
			return "composite"
		end
		if b.to_bn.mod_exp(2,n) != 1
			return "composite"
		end
	end
	return "unknown"
end


numbers = [5,9,9*(2**7)+1,9*(2**9)+1,9*(2**33)+1,9*(2**31)+1,9*(2**134)+1,9*(2**132)+1,9*(2**366)+1,9*(2**368)+1,9*(2**782)+1,9*(2**780)+1]

for i in 0..(numbers.length-1)
	puts Benchmark.measure {
		puts proth(numbers[i],1)
	}
end


def gogo()
	i = 5
	while true
		test = proth(i,64)
		if ((test[0].to_s == "p") && (i.prime?)) || ((test[0].to_s == "c") && !(i.prime?))
			i += 2
			next
		else
			return "hahaha fryd for proth - " + i.to_s + " er slet ikke " + test.to_s
		end
	end
end
puts gogo()