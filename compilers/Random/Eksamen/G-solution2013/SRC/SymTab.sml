(* Symbol Table: Polymorphic for Values and Functions *)
structure SymTab = struct

    exception Duplicate of string (* passing duplicate key to caller *)
    exception Error of string*(int*int)

    fun empty()                 = []

    fun lookup n []             = NONE
      | lookup n ((n1,i1)::tab) = if (n=n1) then SOME i1 else lookup n tab

    fun tryLookup n xs p = case lookup n xs of
                               SOME t => t
                             | NONE => raise Error ("unknown variable " ^ n, p)

    (* (re-)binding names (shadowing possible earlier definitions *)
    fun bind n i stab = (n,i)::stab

    (* binding with a duplicate check (shadowing disallowed) *)
    fun insert n i stab
      = case lookup n stab of
            NONE       => (n,i)::stab
          | SOME thing => raise Duplicate n

end
