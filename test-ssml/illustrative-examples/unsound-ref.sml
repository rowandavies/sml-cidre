
(* The following shows the need for the value restriction on intersection
   introduction.  It shouldn't sort-check.  *)


datatype nat = z | s of nat

(*[ datasort z = z 
         and nz = s of nat ]*)

(*[ val natref :>  z -> z ref  &  nat -> nat ref ]*)
fun natref x = ref x

(*[ val h :> unit -> z ]*)
val h = (fn y =>
  let
    val (r:z ref & nat ref) = natref z
  in
    (r := (s z); !r)
  end)


