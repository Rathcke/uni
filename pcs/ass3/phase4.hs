func a
  | a > 2  = func(a-1) + func(a-2) + func(a-3)
  | a <= 2 = 1
