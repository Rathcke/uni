(* Mosml -- some Moscow ML specific functions *)

val argv      : unit -> string list
val time      : ('a -> 'b) -> ('a -> 'b)
val listDir   : string -> string list
val doubleVec : real -> Word8Vector.vector
val vecDouble : Word8Vector.vector -> real
val floatVec  : real -> Word8Vector.vector
val vecFloat  : Word8Vector.vector -> real
val md5sum    : string -> string

datatype runresult = 
    Success of string
  | Failure of string

val run : string -> string list -> string -> runresult

val systemInfo: string list -> (string * string) list

(* 
   [argv ()] returns the command line strings of the current process.
   Hence List.nth(argv (), 0) is the command used to invoke the SML
   process, List.nth(argv (), 1) is its first argument, and so on.
   We recommend using the SML Basis Library CommandLine structure instead.

   [time f arg] applies f to arg and returns the result; as a side
   effect, it prints the time (cpu, system, and real time) consumed by
   the evaluation.

   [listDir path] returns the list of all files and subdirectories of
   the directory indicated by path.  Raises OS.SysErr in case of failure.

   [doubleVec r] returns an eight-element vector of Word8.word, which
   contains the real number in the IEEE 754 floating-point `double
   format' bit layout stored in big-endian (high byte first) order. 

   [vecDouble v] accepts an eight-element vector v of Word8.word, and
   returns the real number obtained by taking v to be an IEEE 754
   floating-point `double format' number stored in big-endian (high
   byte first) order.  Raises Fail if v is not en eight-element
   vector.

   [floatVec r] returns a four-element vector of Word8.word, which
   contains the real number in the IEEE 754 floating-point `float
   format' bit layout stored in big-endian (high byte first) order.
   Raises Fail if r is not representable as a 32-bit float.

   [vecFloat v] accepts a four-element vector v of Word8.word, and
   returns the real obtained by taking v to be an IEEE 754
   floating-point `float format' number stored in big-endian (high
   byte first) order.  Raises Fail if v is not a four-element vector.

   [md5sum s] computes the 128-bit MD5 checksum of string s and
   returns it as a 22 character base64 string.  

   [run cmd args inp] executes the program cmd with command-line
   arguments args and standard input inp.  Returns Success s where s
   is the program's (standard and error) output as a string, if it
   executed successfully; otherwise returns Failure s where s is its
   (standard and error) output as a string.
       Extreme care should be taken when calling this function in web
   scripts and similar, since the cmd is executed by the shell, so
   even the args can be abused for attacks.

   [systemInfo query] returns a pair (p, v) for each property p in
   query, where v is the value associated with p. If query is the
   empty list, then all properties and values are returned.  The
   property "version" is always guaranteed to have a value associated.
*)
