a = 1024
b = 1024
i = 0
while a != 1
  if (a % 2) == 0
    a /= 2
  else
    a = a*3+1
  end
  i += 1
end
print b.to_s + ' ' + i.to_s
