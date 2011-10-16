
datatype bits = bnil | b0 of bits | b1 of bits

(* We represent natural numbers with the most significant digit first.  *)
val fourmsf = b1 (b0 (b0 bnil))

(*[ datasort natmsf = bnil | b1 of bits ]*)

(*[ stdize <: bits -> natmsf ]*)
fun stdize bnil = bnil
  | stdize (b0 x) = stdize x
  | stdize y = y

(* This is one way to capture the invariant, if the carry is a separate Boolean. *)
datatype natCarry = NC of bits * bool
(*[ datasort validNC = NC of nat * ff | NC of bits * tt ]*)


(*[ incCarry <: nat -> validNC ]*)
fun incCarry bnil = (bnil, true)
(*  | inc (b0 bnil) = (b1 bnil, false) *) (* Can be avoided by carrying in bnil case *)
(*  | inc (b1 bnil) = (b0 bnil, true)  *) (* Can be avoided by carrying in bnil case *)
  | inc (b0 x) = 
      case incCarry x
        of (res, false) => (b0 res, false)
         | (res, true) => (b1 res, false)
  | inc (b1 x) = 
      case incCarry x
        of (res, false) => (b1 res, false)
         | (res, true) => (b0 res, true)

(*[ inc <:  nat -> nat & bits -> bits ]*)
fun inc x = case incCarry x 
              of (res, false) => res
               | (res, true) => b1 res


(*[ inc <:  nat -> pos  &  bits -> bits ]*)
fun inc bnil = b1 bnil
  | inc (b0 x) = b1 x
  | inc (b1 x) = b0 (inc x) 

(*[ plus <: (nat -> nat -> nat) (* & (pos -> pos -> pos) *)
          & (nat -> pos -> pos) & (pos -> nat -> pos) ]*)
fun plus bnil n = n
  | plus m bnil = m
  | plus (b0 m) (b0 n) = b0 (plus m n)
  | plus (b0 m) (b1 n) = b1 (plus m n)
  | plus (b1 m) (b0 n) = b1 (plus m n)
  | plus (b1 m) (b1 n) = b0 (inc (plus m n))
