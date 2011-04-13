
datatype bits = bnil | b0 of bits | b1 of bits

(* We represent natural numbers with the least significant digit first.  *)
val six = b0 (b1 (b1 bnil))

(*[ datasort nat = bnil | b0 of pos | b1 of nat
         and pos =        b0 of pos | b1 of nat ]*)

(*[ stdize <: bits -> nat ]*)
fun stdize bnil = bnil
  | stdize (b0 x) = (case stdize x
                       of bnil => bnil
                        | y => b0 y
                    )
  | stdize (b1 x) = b1 (stdize x)


(*[ inc <:  nat -> pos ]*)
fun inc bnil = b1 bnil
  | inc (b0 x) = b1 x
  | inc (b1 x) = b0 (inc x) 

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


(*
(*[ double <: (nat -> nat) & (pos -> pos) ]*)
fun double n = b0 n

../test-ssml/thesis-examples/bitstring.sml, line 40, column 15:
  fun double n = b0 n
                 ^^^^
  Sort mismatch: bits
      expecting: nat
*)

    (*[ double <: (nat -> nat) & (pos -> pos) ]*)
    fun double bnil = bnil
      | double n = b0 n



