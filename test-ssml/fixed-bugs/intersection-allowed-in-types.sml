(* A tiny example that demonstrates that "&" can be used in types, when it should
   be restricted to sorts.  This shouldn't be very hard to fix, and we'd like to prevent
   programs from sort checking that won't type check with a standard SML compiler.
   - Rowan 14aug02, updated 12sep03. *)

() : unit & unit
