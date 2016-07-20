local
in
datatype token =
    CharA of  int 
  | CharB of  int 
  | EOF of  int 
end;

val Start :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> int;
