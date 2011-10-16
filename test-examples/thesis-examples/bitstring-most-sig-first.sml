
datatype bits = bnil | b0 of bits | b1 of bits

(* We represent natural numbers with the most significant digit first.  *)
val fourmsf = b1 (b0 (b0 bnil))

(*[ datasort natmsf = bnil | b1 of bits ]*)

(*[ datasort b1nat = bnil| b1 of bits ]*)
(*[ datasort carrynat = b1 of bits | b0 of natmsf ]*)
(*[ datasort bitsplus = b1 of bits | b0 of bits ]*)


(*[ stdize <: bits -> natmsf ]*)
fun stdize bnil = bnil
  | stdize (b0 x) = stdize x
  | stdize y = y

(* This always adds an additional "carry" bit at the front. *)
(*[ incCarry <: bits -> bitsplus & b1nat -> carrynat ]*)
fun incCarry bnil = b1 bnil
  | incCarry (b0 x) = b0 (incCarry x)
  | incCarry (b1 x) = 
      case incCarry x
        of b0 y => b0 (b1 y)
         | b1 y => b1 (b0 y)

(*[ inc <:  natmsf -> natmsf & bits -> bits ]*)
fun inc x = case incCarry x 
              of b0 y => y
               | y => y


(*
(*[ plus <: (nat -> nat -> nat) (* & (pos -> pos -> pos) *)
          & (nat -> pos -> pos) & (pos -> nat -> pos) ]*)
fun plus bnil n = n
  | plus m bnil = m
  | plus (b0 m) (b0 n) = b0 (plus m n)
  | plus (b0 m) (b1 n) = b1 (plus m n)
  | plus (b1 m) (b0 n) = b1 (plus m n)
  | plus (b1 m) (b1 n) = b0 (inc (plus m n))

*)