signature RegAlloc =
sig

  exception error of string

  val registerAlloc : Mips.mips list -> string list -> int -> int -> int -> int
		      -> Mips.mips list * string list * int * int

(* registerAlloc takes a list of MIPS instructions, a list of
   registers that are live at the end of the code, three register
   numbers:
     1) The lowest allocatable register (typically 2).
     2) The highest caller-saves register.
     3) The highest allocatable register (typically 25).
   and the number of already spilled variables.  This should be 0 in the initial
   call unless some variables are forced to spill before register allocation.
   Registers up to (and including) the highest caller-saves
   register are assumed to be caller-saves. Those above are assumed to
   be callee-saves.

   registerAlloc returns:
   a modified instruction list where null moves have been removed,
   a list of the variables that are live at entry,
   plus a number indicating the highest used register number.

   The latter can be used for deciding which callee-saves registers
   need to be saved.

   Limitations:

    - Works for a single procedure body only.

    - Assumes all JALs eventually return to the next instruction and
      preserve callee-saves registers when doing so.

    - Does caller-saves preservation only by allocating variables that
      are live across procedure calls to callee-saves registers and
      variables not live across call preferably to caller-saves.

    - Can only remove null moves if they are implemented by ORI (rx,ry,"0").
      Use the pseudo-instruction MOVE (rx,ry) for this.

*)

end
