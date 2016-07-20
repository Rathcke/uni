local
in
datatype token =
    CharA of  int 
  | CharB of  int 
  | EOF of  int 
end;

val A :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> int;
