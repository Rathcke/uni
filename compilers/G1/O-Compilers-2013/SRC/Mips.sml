
(* Mips instruction data type for a subset of instructions   *)
(* understood by the MARS simulator used in the lecture      *)
(* including most pseudo-instructions and directives         *)

datatype mips
 = LABEL of string
 | EQU of string*string
 | GLOBL of string
 | TEXT of string
 | DATA of string
 | SPACE of string
 | ASCII of string
 | ASCIIZ of string
 | ALIGN of string
 | COMMENT of string
 | LA of string*string
 | LUI of string*string
 | ADD of string*string*string
 | ADDI of string*string*string
 | SUB of string*string*string
 | MUL of string*string*string (* low bytes of result in dest., no overflow *)
 | DIV of string*string*string (* quotient in dest., no remainder *)
 | AND of string*string*string
 | ANDI of string*string*string
 | OR of string*string*string
 | ORI of string*string*string
 | XOR of string*string*string
 | XORI of string*string*string
 | SLL of string*string*string
 | SRA of string*string*string
 | SLT of string*string*string
 | SLTI of string*string*string
 | BEQ  of string*string*string
 | BNE  of string*string*string
 | BGEZ of string*string
 | J of string
    (* the list of strings in JAL and JR is used by the register allocator *)
 | JAL of string * string list (* label + arg. reg.s *)
 | JR of string * string list (* jump reg. + result reg.s *)
 | LW of string*string*string (* lw rd,i(rs), encoded as LW (rd,rs,i) *)
 | SW of string*string*string (* sw rd,i(rs) encoded as SW (rd,rs,i) *)
 | LB of string*string*string (* lb rd,i(rs) encoded as LB (rd,rs,i) *)
 | SB of string*string*string (* sb rd,i(rs) encoded as SB (rd,rs,i) *)
 | NOP
 | SYSCALL

fun MOVE (rd,rs) = ORI (rd,rs,"0")

fun LI (rd,v) = ORI (rd,"0",v)

type MipsProg = mips list

fun pp_mips inst
 = case inst of
     LABEL l => l ^ ":"
   | GLOBL s => "\t.globl\t" ^ s
   | TEXT s => "\t.text\t" ^ s
   | DATA s => "\t.data\t" ^ s
   | SPACE s => "\t.space\t" ^ s
   | ASCII s => "\t.ascii\t\"" ^ String.toCString s ^"\""
   | ASCIIZ s => "\t.asciiz\t\"" ^ String.toCString s ^"\""
   | ALIGN s => "\t.align\t" ^ s
   | EQU (l,s) => l ^ "\t=\t" ^ s
   | COMMENT s => "# " ^ s
   | LA (rt,v) => "\tla\t" ^ pp_reg rt ^ ", " ^ v
   | LUI (rt,v) => "\tlui\t" ^ pp_reg rt ^ ", " ^ v
   | ADD (rd,rs,rt) => "\tadd\t" ^ pp_reg rd ^ ", " ^ pp_reg rs ^ ", " ^ pp_reg rt
   | ADDI (rd,rs,v) => "\taddi\t" ^ pp_reg rd ^ ", " ^ pp_reg rs ^ ", " ^ v
   | SUB (rd,rs,rt) => "\tsub\t" ^ pp_reg rd ^ ", " ^ pp_reg rs ^ ", " ^ pp_reg rt
   | MUL (rd,rs,rt) => "\tmul\t" ^ pp_reg rd ^ ", " ^ pp_reg rs ^ ", " ^ pp_reg rt
   | DIV (rd,rs,rt) => "\tdiv\t" ^ pp_reg rd ^ ", " ^ pp_reg rs ^ ", " ^ pp_reg rt
   | AND (rd,rs,rt) => "\tand\t" ^ pp_reg rd ^ ", " ^ pp_reg rs ^ ", " ^ pp_reg rt
   | ANDI (rd,rs,v) => "\tandi\t" ^ pp_reg rd ^ ", " ^ pp_reg rs ^ ", " ^ v
   | OR (rd,rs,rt) => "\tor\t" ^ pp_reg rd ^ ", " ^ pp_reg rs ^ ", " ^ pp_reg rt
   | ORI (rd,rs,v) => "\tori\t" ^ pp_reg rd ^ ", " ^ pp_reg rs ^ ", " ^ v
   | XOR (rd,rs,rt) => "\txor\t" ^ pp_reg rd ^ ", " ^ pp_reg rs ^ ", " ^ pp_reg rt
   | XORI (rd,rs,v) => "\txori\t" ^ pp_reg rd ^ ", " ^ pp_reg rs ^ ", " ^ v
   | SLL (rd,rt,v) => "\tsll\t" ^ pp_reg rd ^ ", " ^ pp_reg rt ^ ", " ^ v
   | SRA (rd,rt,v) => "\tsra\t" ^ pp_reg rd ^ ", " ^ pp_reg rt ^ ", " ^ v
   | SLT (rd,rs,rt) => "\tslt\t" ^ pp_reg rd ^ ", " ^ pp_reg rs ^ ", " ^ pp_reg rt
   | SLTI (rd,rs,v) => "\tslti\t" ^ pp_reg rd ^ ", " ^ pp_reg rs ^ ", " ^ v
   | BEQ (rs,rt,v) => "\tbeq\t" ^ pp_reg rs ^ ", " ^ pp_reg rt ^ ", " ^ v
   | BNE (rs,rt,v) => "\tbne\t" ^ pp_reg rs ^ ", " ^ pp_reg rt ^ ", " ^ v
   | BGEZ(rs,rt)   => "\tbgez\t"^ pp_reg rs ^ ", " ^ pp_reg rt
   | J l => "\tj\t" ^ l
   | JAL (l,argRegs) => "\tjal\t" ^ l
   | JR (r,resRegs) => "\tjr\t" ^ pp_reg r
   | LW (rd,rs,v) => "\tlw\t" ^ pp_reg rd ^ ", " ^ v ^ "(" ^ pp_reg rs ^ ")"
   | SW (rd,rs,v) => "\tsw\t" ^ pp_reg rd ^ ", " ^ v ^ "(" ^ pp_reg rs ^ ")"
   | LB (rd,rs,v) => "\tlb\t" ^ pp_reg rd ^ ", " ^ v ^ "(" ^ pp_reg rs ^ ")"
   | SB (rd,rs,v) => "\tsb\t" ^ pp_reg rd ^ ", " ^ v ^ "(" ^ pp_reg rs ^ ")"
   | NOP => "\tnop"
   | SYSCALL => "\tsyscall"

and pp_reg r =
  if numerical r then "$" ^ r else r

and numerical s =
  foldl (fn (c,i) => i andalso ord c >= ord #"0" andalso ord c <= ord #"9")
        true (explode s)

and intOfString s =
  foldl ( fn (c,i) => 10*i + ord c- ord #"0" ) 0 (explode s)

(* pp_mips_list prints a list of instructions, one per line, *)
(* so that the assembler can read them                       *)

and pp_mips_list is =
    String.concat (List.map (fn i => pp_mips i ^"\n") is)
