(* Array -- SML Basis Library *)

prim_EQtype 'a array
type 'a vector = 'a Vector.vector

val maxLen   : int

val array    : int * '_a -> '_a array
val tabulate : int * (int -> '_a) -> '_a array
val fromList : '_a list -> '_a array

val length   : 'a array -> int
val sub      : 'a array * int -> 'a
val update   : 'a array * int * 'a  -> unit
val vector   : 'a array -> 'a vector

val copy     : {src: 'a array,  dst: 'a array, di: int} -> unit
val copyVec  : {src: 'a vector, dst: 'a array, di: int} -> unit

val find     : ('a -> bool) -> 'a array -> 'a option
val exists   : ('a -> bool) -> 'a array -> bool
val all      : ('a -> bool) -> 'a array -> bool

val app      : ('a -> unit) -> 'a array -> unit
val foldl    : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
val foldr    : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
val modify   : ('a -> 'a) -> 'a array -> unit

val findi    : (int * 'a -> bool) -> 'a array -> (int * 'a) option
val appi     : (int * 'a -> unit) -> 'a array -> unit
val foldli   : (int * 'a * 'b -> 'b) -> 'b -> 'a array -> 'b
val foldri   : (int * 'a * 'b -> 'b) -> 'b -> 'a array -> 'b
val modifyi  : (int * 'a -> 'a) -> 'a array -> unit

val collate  : ('a * 'a -> order) -> 'a array * 'a array -> order

(* 
   ['ty array] is the type of one-dimensional, mutable, zero-based
   constant-time-access arrays with elements of type 'ty.  Type 
   'ty array admits equality even if 'ty does not.  Arrays a1 and a2 
   are equal if both were created by the same call to a primitive
   (array, tabulate, fromList).

   Functions working on a slices (contiguous subsequence) of an array
   are found in the ArraySlice structure.

   [maxLen] is the maximal number of elements in an array.

   [array(n, x)] returns a new array of length n whose elements are all x.
   Raises Size if n<0 or n>maxLen.

   [tabulate(n, f)] returns a new array of length n whose elements
   are f 0, f 1, ..., f (n-1), created from left to right.  Raises
   Size if n<0 or n>maxLen.

   [fromList xs] returns an array whose elements are those of xs.
   Raises Size if length xs > maxLen.

   [length a] returns the number of elements in a.

   [sub(a, i)] returns the i'th element of a, counting from 0.  
   Raises Subscript if i<0 or i>=length a.  To make `sub' infix, use
   the declaration 
                             infix 9 sub

   [update(a, i, x)] destructively replaces the i'th element of a by x.
   Raises Subscript if i<0 or i>=length a.

   [copy{src, dst, di}] destructively copies the array src to dst,
   starting at index di.  
   Raises Subscript if di<0, or if di + length src > length dst.

   [copyVec{src, dst, di}] destructively copies the vector to dst,
   starting at index di.  
   Raises Subscript if di<0, or if di + Vector.length src > length dst.

   [find p a] applies p to each element x of a, from left to right,
   until p(x) evaluates to true; returns SOME x if such an x exists,
   otherwise NONE.

   [exists p a] applies p to each element x of a, from left to right,
   until p(x) evaluates to true; returns true if such an x exists,
   otherwise false.

   [all p a] applies p to each element x of a, from left to right,
   until p(x) evaluates to false; returns false if such an x exists,
   otherwise true.

   [foldl f e a] folds function f over a from left to right.  That is,
   computes f(a[len-1], f(a[len-2], ..., f(a[1], f(a[0], e)) ...)),
   where len is the length of a.

   [foldr f e a] folds function f over a from right to left.  That is,
   computes f(a[0], f(a[1], ..., f(a[len-2], f(a[len-1], e)) ...)),
   where len is the length of a.

   [app f a] applies f to a[j] for j=0,1,...,length a-1.

   [modify f a] applies f to a[j] and updates a[j] with the result
   f(a[j]) for j=0,1,...,length a-1. 

   The following iterators generalize the above ones by passing also
   the index j to the function being iterated.

   [findi p a] applies f to successive pairs (j, a[j]) for j=0,1,...,n-1, 
   until p(j, a[j]) evaluates to true; returns SOME (j, a[j]) if such
   a pair exists, otherwise NONE.

   [foldli f e a] folds function f over the array from left to right.
   That is, computes f(n-1, a[n-1], f(..., f(1, a[1], f(0, a[0], e)) ...)).  

   [foldri f e a] folds function f over the array from right to left.  
   That is, computes f(0, a[0], f(1, a[1], ..., f(n-1, a[n-1], e) ...)).

   [appi f a] applies f to successive pairs (j, a[j]) for j=0,1,...,n-1.  

   [modifyi f a] applies f to (j, a[j]) and updates a[j] with the
   result f(j, a[j]) for j=0,1,...,n-1.

   [collate cmp (xs, ys)] returns LESS, EQUAL or GREATER according as
   xs precedes, equals or follows ys in the lexicographic ordering on
   arrays induced by the ordering cmp on elements.  
*)
