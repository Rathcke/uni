var xs = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
var squares = [ for (x of xs) x * x ];
var evens = [ for (x of xs) if (x % 2 === 0) x ];
var many_a = [ for (x of xs) for (y of xs) 'a' ];
var hundred = [ for (i of [0])
                for (x of xs)
                for (y of xs) i = i + 1 ];

