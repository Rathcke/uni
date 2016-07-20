signature Type =
sig

  exception Error of string*(int*int)

  datatype ExpectType = SomeArray of TpAbSyn.BasicType
                      | KnownType of TpAbSyn.Type
                      | UnknownType

  (* build static function table and export it *)
  val functionTable : (string * (TpAbSyn.Type list * TpAbSyn.Type option)) list ref

  val typeCheckPgm : AbSyn.Prog -> TpAbSyn.Prog

(* The type checker function exported here returns a modified syntax tree
  TpAbSyn in which all expressions are typeable via helper function `TpAbSyn.typeOfExp'
  The types introduced by the type checker are needed for example for array 
  allocation in the compiler (to know array memory size).

  If a type error is detected, the exception will be raised. It is up to the
  implementation whether all type errors are collected in the string (ignoring
  the position) or the exception is raised immediately.
*)

(* for debugging... *)
  val typeCheckExp :  ( (string * TpAbSyn.Type) list * AbSyn.Exp * ExpectType) -> TpAbSyn.Exp

end
