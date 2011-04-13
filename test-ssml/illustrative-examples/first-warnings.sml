
(*[ datasort tt = true and ff = false ]*)

datatype t = C of bool
(*[ datasort s = C of tt | C of ff ]*)

(*[ val g :> ff -> unit ]*)
fun g x = ()

(* There is an error here, it should be reported. The warning occurs for a different case
   so is omitted. *)
fun f (C x) = let val true = x 
	      in g x 
	      end

(* There is an error here, it should be reported, along with the warning.  *)
fun f (C x) = let val false = x 
	      in g x 
	      end

(* There is only a warning here, so it should be reported.  *)
fun f (C x) = let val true = x 
	      in x 
	      end
