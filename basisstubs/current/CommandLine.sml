(* CommandLine -- SML Basis Library *)

structure CommandLine:COMMAND_LINE =
struct(*[ assumesig COMMAND_LINE ]*)
    val name      : unit -> string  = fn _ => raise Match
    val arguments : unit -> string list = fn _ => raise Match
  end

(* 
   [name ()] returns the name used to start the current process.

   [arguments ()] returns the command line arguments of the current process.
   Hence List.nth(arguments (), 0) is the first argument.
*)
