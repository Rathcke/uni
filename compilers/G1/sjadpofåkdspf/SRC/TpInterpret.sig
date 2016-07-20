signature TpInterpret = sig
    type Pos = TpAbSyn.Pos
    type v = TpAbSyn.Value
    type t = TpAbSyn.Type
    type e = TpAbSyn.Exp
    type b = TpAbSyn.StmtBlock
    type d = TpAbSyn.Dec
    type f = TpAbSyn.FunDec

    exception Error of string*(int*int)

    val buildFtab: f list -> (string * f) list

    val evalExp  : e * (string * v ref) list * (string * f) list -> v
    val callFun  : ( t option * string * d list * b * Pos ) * v list * e list * 
                   (string * v ref) list * (string * f) list * Pos -> v option 

    val execPgm : f list -> v option
end

